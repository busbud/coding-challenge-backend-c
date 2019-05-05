-module(busbudcc_db).

%% API
-export([pool_name/0,
         select_query/2]).

%% Macros
-define(POOL_NAME, budbudcc_db_pool).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec pool_name() -> atom().
pool_name() ->
  ?POOL_NAME.

-spec select_query(string(), []) -> [map()].
select_query(Query, Params) ->
  poolboy:transaction(?POOL_NAME,
                      fun
                        (Worker) ->
                          busbudcc_db_pool_worker:select_query(Worker, Query, Params)
                      end).
