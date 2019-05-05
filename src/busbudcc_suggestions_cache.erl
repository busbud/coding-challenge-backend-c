-module(busbudcc_suggestions_cache).

%% API
-export([start_link/0,
         suggest/1]).

%% Macros
-define(CACHE_NAME, busbudcc_suggestions_cache).
-define(CACHE_SIZE, 16). % 16MB
-define(CACHE_TTL, 300000). % 300000ms = 5 minutes

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  ecache_server:start_link(?CACHE_NAME, busbudcc_cities_entity, suggest, ?CACHE_SIZE, ?CACHE_TTL).

-spec suggest(map()) -> {ok, [map()]} | {error, binary()}.
suggest(Params) ->
  ecache:get(?CACHE_NAME, Params).
