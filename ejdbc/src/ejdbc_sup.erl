%%%-------------------------------------------------------------------
%% @doc ejdbc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ejdbc_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Name) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Name]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%init([]) ->
%    {ok, { {one_for_all, 0, 1}, []} }.

init([Name]) ->
    RestartStrategy = simple_one_for_one,
    MaxR = 0,
    MaxT = 3600,
    StartFunc = {ejdbc, start_link_sup, []},
    Restart = temporary, % E.g. should not be restarted
    Shutdown = 7000,
    Modules = [ejdbc],
    Type = worker,
    ChildSpec = {Name, StartFunc, Restart, Shutdown, Type, Modules},
    {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}.


%%====================================================================
%% Internal functions
%%====================================================================
