-module(busbudcc_cities_entity_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         t_suggest/1,
         t_suggest_with_location/1,
         t_suggest_cached_with_no_query/1,
         t_suggest_cached_with_invalid_location/1]).

-include_lib("common_test/include/ct.hrl").

groups() ->
  [{suggest,
    [],
    [t_suggest,
     t_suggest_with_location]},
   {suggest_cached,
    [t_suggest_cached_with_no_query,
     t_suggest_cached_with_invalid_location]}].

suite() ->
  [{timetrap, {minutes, 1}},
   {require, {busbudcc, [webserver_port]}}].

all() ->
  [{group, suggest},
   {group, suggest_cached}].

init_per_suite(Config) ->
  busbudcc_test_helper:set_env_vars(),
  {ok, _} = application:ensure_all_started(busbudcc),
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_Case, _Config) ->
  ok.

%%%===================================================================
%% suggest
%%%===================================================================

t_suggest(_Config) ->
  [#{name := <<"London, ", _RestName/binary>>} | _Rest] =
    busbudcc_cities_entity:suggest({unlocated, "Londo"}).

t_suggest_with_location(_Config) ->
  [#{name := <<"London, ", _RestName/binary>>} | _Rest] =
    busbudcc_cities_entity:suggest({located, "londo", -79.4163, 43.70011}).

%%%===================================================================
%% suggest_cached
%%%===================================================================

t_suggest_cached_with_no_query(_Config) ->
  {error, _ErrMsg} = busbudcc_cities_entity:suggest_cached(#{}).

t_suggest_cached_with_invalid_location(_Config) ->
  {error, _ErrMsg} =
    busbudcc_cities_entity:suggest_cached(#{<<"q">> => <<"Londo">>,
                                            <<"latitude">> => <<"invalid!">>,
                                            <<"longitude">> => <<"-79.4163">>}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
