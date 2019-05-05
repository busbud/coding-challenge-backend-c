-module(busbudcc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupFlags = #{strategy => one_for_one,
               intensity => 1,
               period => 5},
  Webserver = #{id => webserver,
                start => {busbudcc_webserver, start_link, []},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [busbudcc_webserver]},
  DbPoolName = busbudcc_db:pool_name(),
  {ok, DbPoolSize} = application:get_env(busbudcc, database_pool_size),
  DbPool = poolboy:child_spec(DbPoolName,
                              [{name, {local, DbPoolName}},
                               {worker_module, busbudcc_db_pool_worker},
                               {size, DbPoolSize},
                               {max_overflow, ceil(DbPoolSize / 2)}],
                              []),
  {ok, {SupFlags, [Webserver, DbPool]}}.

%%====================================================================
%% Internal functions
%%====================================================================
