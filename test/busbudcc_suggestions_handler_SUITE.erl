-module(busbudcc_suggestions_handler_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         t_suggestions/1,
         t_suggestions_not_found/1,
         t_suggestions_invalid_content/1]).

-include_lib("common_test/include/ct.hrl").

groups() ->
  [{suggestions,
    [],
    [t_suggestions,
     t_suggestions_not_found,
     t_suggestions_invalid_content]}].

suite() ->
  [{timetrap, {minutes, 1}},
   {require, {busbudcc, [webserver_port]}}].

all() ->
  [{group, suggestions}].

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
%% suggestions
%%%===================================================================

t_suggestions(_Config) ->
  Response = busbudcc_test_helper:request(get, suggestions, #{<<"q">> => <<"non-existent-city">>}),
  Body = busbudcc_test_helper:response_body(Response),
  200 = busbudcc_test_helper:response_status(Response),
  #{<<"suggestions">> := []} = jsx:decode(Body, [return_maps]).

t_suggestions_not_found(_Config) ->
  Response = busbudcc_test_helper:request(get, suggestions, #{<<"q">> => <<"non-existent-city">>}),
  Body = busbudcc_test_helper:response_body(Response),
  200 = busbudcc_test_helper:response_status(Response),
  #{<<"suggestions">> := []} = jsx:decode(Body, [return_maps]).

t_suggestions_invalid_content(_Config) ->
  Response = busbudcc_test_helper:request(get, suggestions, #{}),
  Body = busbudcc_test_helper:response_body(Response),
  422 = busbudcc_test_helper:response_status(Response),
  #{<<"error">> := _ErrMsg} = jsx:decode(Body, [return_maps]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
