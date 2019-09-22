

-module(ejdbc).

-behaviour(gen_server).

%% API --------------------------------------------------------------------
-export([start/0, start/1, stop/0, connect/5]).

%%-------------------------------------------------------------------------
%% supervisor callbacks
-export([start_link_sup/1]).

%%-------------------------------------------------------------------------
%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3]).


%%--------------------------------------------------------------------------
%% Internal state
-record(state, {erlang_port,                 % The port to the c-program
	  tcp_port, 
	  server_socket,
		reply_to,		     % gen_server From parameter 
		owner,                       % Pid of the connection owner
		result_set = undefined,      % exists | undefined
		auto_commit_mode = on,       % on | off
		%% Indicates if first, last and "select absolut"
		%% is supported by the odbc driver.
		absolute_pos,                % true | false  
		%% Indicates if prev and "select relative"
		%% is supported by the odbc driver.
		relative_pos,                % true | false
		scrollable_cursors,      % on | off
		%% connecting | connected | disconnecting
		state = connecting,	    
		%% For timeout handling
		pending_request,      
		num_timeouts = 0,
		listen_sockets,
		sup_socket,
		odbc_socket
	       }).


%%--------------------------------------------------------------------
%% Function: start([, Type]) -> ok
%%
%%  Type =  permanent | transient | temporary
%%
%% Description: Starts the inets application. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
start() -> 
    application:start(ejdbc).

start(Type) -> 
    application:start(ejdbc, Type).

%%--------------------------------------------------------------------
%% Function: stop() -> ok
%%
%% Description: Stops the ejdbc application.
%%--------------------------------------------------------------------
stop() -> 
    application:stop(ejdbc).

%%-------------------------------------------------------------------------
%% connect(Driver, Url, Username, Password, Options) -> {ok, ConnectionReferense} |
%%                                                      {error, Reason}
%% Description: Spawns an erlang control process that will open a port
%%              to a java-process that uses the JDBC API to open a connection
%%              to the database.
%%-------------------------------------------------------------------------
connect(Driver, Url, Username, Password, Options) when is_list(Options) ->
   %% Spawn the erlang control process.
   try supervisor:start_child(ejdbc_sup, [[{client, self()}]]) of
       {ok, Pid} ->
           connect(Pid, {Driver, Url, Username, Password}, Options);
       {error, Reason} ->
           {error, Reason}
   catch
       exit:{noproc, _} ->
           {error, ejdbc_not_started}
   end.


%%--------------------------------------------------------------------------
%% start_link_sup(Args) -> {ok, Pid} | {error, Reason} 
%%                                    
%% Description: Callback function for the ejdbc supervisor. It is called 
%%            : when connect/5 calls supervisor:start_child/2 to start an 
%%            : instance of the erlang ejdbc control process.
%%--------------------------------------------------------------------------
start_link_sup(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%-------------------------------------------------------------------------
%% init(Args) -> {ok, State} | {ok, State, Timeout} | {stop, Reason}
%% Description: Initiates the ejdbc process that manages the connection
%%              and starts the port-program that use the jdbc driver
%%              to communicate with the database.
%%-------------------------------------------------------------------------
init(Args) ->
   process_flag(trap_exit, true),
    {value, {client, ClientPid}} = lists:keysearch(client, 1, Args),
    erlang:monitor(process, ClientPid),
    %% Start the port program (a java program) that utilizes the jdbc driver
    case os:find_executable("java") of
      JavaFileName when is_list(JavaFileName)->
         JdbcServerJar = filename:nativename(filename:join(code:priv_dir(ejdbc), "jdbcserver.jar")),
         JinterfaceJar = filename:nativename(filename:join(code:priv_dir(ejdbc), "jinterface-1.6.1.jar")),
         Cp = case os:getenv("CLASSPATH") of
                Classpath when is_list(Classpath) ->
                   JdbcServerJar ++ ":" ++ JinterfaceJar ++ ":" ++ Classpath;
                false ->
                   JdbcServerJar ++ ":" ++ JinterfaceJar
              end,
         PortSettings = [{line, 100}, {args, ["-cp", Cp, "io.github.github.JdbcServer"]}],
         Port = open_port({spawn_executable, JavaFileName}, PortSettings),
         TclPort = receive
            {Port,{Flag, {eol, "port=" ++ Line}}} ->
                list_to_integer(Line);
            Any ->
                error
         end,
         {ok, ServerSocket} = gen_tcp:connect("localhost",TclPort,
             [binary, {mode, binary}, {active, false}, {send_timeout, 5000}]),
         State = #state{
             erlang_port = Port,
             tcp_port = TclPort,
             server_socket = ServerSocket,
             owner = ClientPid
         },
         {ok, State};
      false ->
         {stop, java_program_executable_not_found}
    end.

%%--------------------------------------------------------------------------
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State}               |
%%                                      {noreply, State, Timeout}      |
%%                                      {stop, Reason, Reply, State}   |
%%                                      {stop, Reason, Reply, State}     
%% Description: Handle incoming requests. Only requests from the process
%%              that created the connection are allowed in order to preserve
%%              the semantics of result sets.
%% Note: The order of the function clauses is significant.
%%--------------------------------------------------------------------------
handle_call(ping, From, State) ->
    {reply, {pong, State#state.tcp_port, State#state.server_socket}, State};

handle_call({Client, Msg, Timeout}, From, State = 
	    #state{owner = Client, reply_to = undefined})  ->
    handle_msg(Msg, Timeout, State#state{reply_to = From}).


handle_msg({connect, ConnectionValues, Options}, Timeout, State) ->
    jdbc_send(State#state.server_socket, 1, ConnectionValues),
    {reply, ok, State}.

%%-------------------------------------------------------------------------
%% terminate/2 and code_change/3
%%--------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%%%========================================================================
%%% Internal functions
%%%========================================================================

connect(ConnectionReferense, ConnectionValues, Options) ->
     TimeOut = infinity,
     %% Send request, to open a database connection, to the control process.
    case call(ConnectionReferense, {connect, ConnectionValues, Options}, TimeOut) of
	    ok ->
	      {ok, ConnectionReferense};
	    Error ->
	      Error
    end.
%     {ok, ConnectionReferense}.

%%-------------------------------------------------------------------------
call(ConnectionReference, Msg, Timeout) ->
    Result = (catch gen_server:call(ConnectionReference, {self(), Msg, Timeout}, infinity)),
    case Result of
      %% Normal case, the result from the port-program has directly 
      %% been forwarded to the client
      Binary when is_binary(Binary) -> 
           decode(Binary);
      timeout -> 
           exit(timeout);
      {'EXIT', _} ->
           {error, connection_closed};
      %% At some occasions the erlang control process will have an
      %% answer that was not directly received from the port-program.
      Term ->  
        Term
    end.

%%-------------------------------------------------------------------------
decode(Binary) ->
    case binary_to_term(Binary) of
      [ResultSet | []] -> 
         ResultSet;
      param_badarg ->
        exit({badarg, ejdbc, param_query, 'Params'}); 
      MultipleResultSets_or_Other ->
         MultipleResultSets_or_Other
    end.

%---------------------------------------------------------------------------
%% Catch all - throws away unknown messages (This could happen by "accident"
%% so we do not want to crash, but we make a log entry as it is an
%% unwanted behaviour.) 
handle_info(Info, State) ->
    Report = io_lib:format("JDBC: received: ~p~n", [Info]),
    io:fwrite(Report),
    % error_logger:error_report(Report),
    {noreply, State}.


%%-------------------------------------------------------------------------
jdbc_send(Socket, Cmd, Msg) when is_integer(Cmd) -> %% Note currently all allowed messages are lists
    Bindata = term_to_binary(Msg),
    Binsize = byte_size(Bindata),
    Senddata = [<<Cmd:8, Binsize:16>>, Bindata],
    Report = io_lib:format("JDBC send: ~p~n", [Senddata]),
    io:fwrite(Report),
    % error_logger:error_report(Report),
    %io:fwrite(Senddata),
    %ok = gen_tcp:send(Socket, <<2:8, Binsize:16>>),
    %ok = gen_tcp:send(Socket, Bindata),
    ok = gen_tcp:send(Socket, Senddata),
    ok = inet:setopts(Socket, [{active, once}]).
