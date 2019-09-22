

-module(ejdbc).

-behaviour(gen_server).

%% API --------------------------------------------------------------------
-export([start/0, start/1, stop/0, 
        connect/5, disconnect/1, execute_query/2, execute_update/2]).

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
%% disconnect(ConnectionReferense) -> ok | {error, Reason}
%%                                    
%% Description: Disconnects from the database and terminates both the erlang
%%              control process and the database handling java-process. 
%%--------------------------------------------------------------------------
disconnect(ConnectionReference) when is_pid(ConnectionReference)->
    case call(ConnectionReference, disconnect, 5000) of 
      {error, connection_closed} ->
          %% If the connection has already been closed the effect of
          %% disconnect has already been acomplished
          ok; 
      %% Note a time out of this call will return ok, as disconnect
      %% will always succeed, the time out is to make sure
      %% the connection is killed brutaly if it will not be shut down
      %% gracefully.
      ok ->
          ok;
      %% However you may receive an error message as result if you try to
      %% disconnect a connection started by another process.
      Other ->
          Other
    end. 

%%--------------------------------------------------------------------------
%% execute_query(ConnectionReferense, Sql) -> {ok, data} | {error, Reason}
%%                                    
%% Description: Execute sql query and return rows
%%--------------------------------------------------------------------------
execute_query(ConnectionReference, Sql) when is_pid(ConnectionReference)->
    call(ConnectionReference, {execute_query, Sql}, infinity).

%%--------------------------------------------------------------------------
%% execute_update(ConnectionReferense, Sql) -> {ok, row_count} | {error, Reason}
%%                                    
%% Description: Execute sql query updates and SQL Data Manipulation Language (DML)
%%--------------------------------------------------------------------------
execute_update(ConnectionReference, Sql) when is_pid(ConnectionReference)->
    call(ConnectionReference, {execute_update, Sql}, infinity).


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
             [binary, {mode, binary}, {active, true}, {send_timeout, 5000}]),
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
    {noreply, State};

handle_msg(disconnect, Timeout, State) ->
    jdbc_send(State#state.server_socket, 2, <<>>),
    {noreply, State#state{state = disconnecting}, Timeout};

handle_msg({execute_query, Sql}, Timeout, State) ->
    jdbc_send(State#state.server_socket, 3, Sql),
    {noreply, State, Timeout};

handle_msg({execute_update, Sql}, Timeout, State) ->
    jdbc_send(State#state.server_socket, 4, Sql),
    {noreply, State, Timeout}.

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

%%-------------------------------------------------------------------------
call(ConnectionReference, Msg, Timeout) ->
    Result = (catch gen_server:call(ConnectionReference, {self(), Msg, Timeout}, infinity)),
    case Result of
      ok ->
         ok;
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
   case Binary of
     <<2, Size:32, ErrorMessage/binary>> -> 
        {error, ErrorMessage};
     <<3, Size:32, Data/binary>> -> 
        {ok, binary_to_term(<<131,Data/binary>>)};
     BinaryData ->
        case binary_to_term(BinaryData) of
          [ResultSet | []] -> 
             ResultSet;
          param_badarg ->
            exit({badarg, ejdbc, param_query, 'Params'}); 
          MultipleResultSets_or_Other ->
             MultipleResultSets_or_Other
        end
    end.

%%--------------------------------------------------------------------------
%% handle_info(Msg, State) -> {noreply, State} | {noreply, State, Timeout} |
%%            {stop, Reason, State}
%% Description: Handles timouts, replys from the port-program and EXIT and
%%    down messages.
%% Note: The order of the function clauses is significant.
%%--------------------------------------------------------------------------
handle_info({tcp, Socket, BinData}, State = #state{state = connecting, 
            reply_to = From,
            server_socket = Socket}) ->
    %Report = io_lib:format("JDBC: BinData: ~p~n", [BinData]),
    %io:fwrite(Report),
    
    case BinData of
      % Simple ok
      <<1>> ->
          gen_server:reply(From, ok), 
          {noreply, State#state{state = connected, reply_to = undefined}};
      %<<2>> ->
         %{ok, Len} = gen_tcp:recv(Socket, 4),
         %<<A, B, C, D>> = Len,
         %io:fwrite(io_lib:format("JDBC: recv: ~p~n", [Len])),
         %{ok, Message} = gen_tcp:recv(Socket, D),
         %io:fwrite(io_lib:format("JDBC: recv: ~p~n", [Message])),
         %gen_server:reply(From, <<2, Len/binary, Message/binary>>);
       %  gen_server:reply(From, <<2>>),
       %  {noreply, State};
      Message ->
          Full = unpack(Socket, Message),

          % {noreply, State}
          gen_server:reply(From, Full), 
          {noreply, State#state{reply_to = undefined}}
          % {stop, normal, State#state{reply_to = undefined}}
    end;

handle_info({tcp, Socket, BinData}, State = #state{state = disconnecting,
               reply_to = From}) ->
    %Report = io_lib:format("JDBC: BinData: ~p~n", [BinData]),
    %io:fwrite(Report),
    %% The connection will always be closed 
    gen_server:reply(From, ok),  
    case BinData of
      <<1>> -> 
          ok;
      Message ->
          Report =  io_lib:format("JDBC could not end connection "  
                    "gracefully due to ~p~n", [Message]),
          error_logger:error_report(Report)
    end,
    
    {stop, normal, State#state{reply_to = undefined}};

handle_info({tcp, Socket, BinData}, State = #state{state = connected,
               reply_to = From}) ->
    %Report = io_lib:format("JDBC: BinData: ~p~n", [BinData]),
    %io:fwrite(Report),
    case BinData of
      <<1>> -> 
          gen_server:reply(From, ok);
      Message ->
          Full = unpack(Socket, Message),
          %io:fwrite(io_lib:format("data >> ~p~n", [Full])),
          gen_server:reply(From, Full)
    end,
    {noreply, State#state{reply_to = undefined}};

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
    %Report = io_lib:format("JDBC send: ~p~n", [Senddata]),
    %io:fwrite(Report),
    % error_logger:error_report(Report),
    %io:fwrite(Senddata),
    %ok = gen_tcp:send(Socket, <<2:8, Binsize:16>>),
    %ok = gen_tcp:send(Socket, Bindata),
    ok = gen_tcp:send(Socket, Senddata),
    ok = inet:setopts(Socket, [{active, once}]).

%%-------------------------------------------------------------------------
unpack(Socket, <<X>>) ->
    {ok, Len} = gen_tcp:recv(Socket, 4),
    unpack(Socket, <<X, Len/binary>>);
unpack(Socket, <<X, A>>) ->
    {ok, Len} = gen_tcp:recv(Socket, 3),
    unpack(Socket, <<X, A, Len/binary>>);
unpack(Socket, <<X, A, B>>) ->
    {ok, Len} = gen_tcp:recv(Socket, 2),
    unpack(Socket, <<X, A, B, Len/binary>>);
unpack(Socket, <<X, A, B, C>>) ->
    {ok, Len} = gen_tcp:recv(Socket, 1),
    unpack(Socket, <<X, A, B, C, Len/binary>>);
unpack(Socket, <<X, A, B, C, D>>) ->
    Len = C*256 + D,
    {ok, Rest} = gen_tcp:recv(Socket, Len),
    <<X, A, B, C, D, Rest/binary>>;
unpack(Socket, Data = <<X, Len:32, Rest/binary>>) when byte_size(Rest) == Len ->
    Data.
