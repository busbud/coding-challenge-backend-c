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
         t_suggest_with_no_query/1,
         t_suggest_with_invalid_location/1]).

-include_lib("common_test/include/ct.hrl").

groups() ->
  [{suggest,
    [],
    [t_suggest,
     t_suggest_with_location,
     t_suggest_with_no_query,
     t_suggest_with_invalid_location]}].

suite() ->
  [{timetrap, {minutes, 1}},
   {require, {busbudcc, [webserver_port]}}].

all() ->
  [{group, suggest}].

init_per_suite(Config) ->
  busbudcc_test_helper:set_env_vars(),
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_Case, Config) ->
  Connection = busbudcc_db:create_connection(),
  busbudcc_db:begin_transaction(Connection),
  [{connection, Connection} | Config].

end_per_testcase(_Case, Config) ->
  Connection = ?config(connection, Config),
  busbudcc_db:abort_transaction(Connection).

%%%===================================================================
%% suggest
%%%===================================================================

t_suggest(Config) ->
  Connection = ?config(connection, Config),
  {ok, [#{name := <<"London, ", _RestName/binary>>, score := 0.9} | _Rest]} =
    busbudcc_cities_entity:suggest(Connection, #{<<"q">> => <<"Londo">>}).

t_suggest_with_location(Config) ->
  Connection = ?config(connection, Config),
  {ok, [#{name := <<"London, Canada">>} | _Rest]} =
    busbudcc_cities_entity:suggest(Connection, #{<<"q">> => <<"Londo">>,
                                                 <<"latitude">> => <<"43.70011">>,
                                                 <<"longitude">> => <<"-79.4163">>}).

t_suggest_with_no_query(Config) ->
  Connection = ?config(connection, Config),
  {error, _ErrMsg} =
    busbudcc_cities_entity:suggest(Connection, #{}).

t_suggest_with_invalid_location(Config) ->
  Connection = ?config(connection, Config),
  {error, _ErrMsg} =
    busbudcc_cities_entity:suggest(Connection, #{<<"q">> => <<"Londo">>,
                                                 <<"latitude">> => <<"invalid!">>,
                                                 <<"longitude">> => <<"-79.4163">>}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
