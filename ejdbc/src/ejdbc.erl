

-module(ejdbc).

-behaviour(gen_server).

%% API --------------------------------------------------------------------
-export([start/0, start/1, stop/0, connect/5]).

%%-------------------------------------------------------------------------
%% supervisor callbacks
-export([start_link_sup/1]).

%%-------------------------------------------------------------------------
%% gen_server callbacks
-export([init/1, handle_call/3, terminate/2, code_change/3]).


%%--------------------------------------------------------------------------
%% Internal state
-record(state, {erlang_port,                 % The port to the c-program
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
   {ok, []}.

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
    {reply, pong, State}.


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
    case call(ConnectionReferense, {connect, ConnectionValues}, TimeOut) of
	    ok ->
	      {ok, ConnectionReferense};
	    Error ->
	      Error
    end.

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
