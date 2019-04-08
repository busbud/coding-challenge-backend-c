-module(busbudcc_webserver_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         t_not_found/1]).

-include_lib("common_test/include/ct.hrl").

%% Macros
-define(API_URL, "http://localhost").

groups() ->
  [{not_found,
    [],
    [t_not_found]}].

suite() ->
  [{timetrap, {minutes, 1}},
   {require, {busbudcc, [webserver_port]}}].

all() ->
  [{group, not_found}].

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
%% not_found
%%%===================================================================

t_not_found(_Config) ->
  Response = busbudcc_test_helper:request(get, "/blah", #{}),
  404 = busbudcc_test_helper:response_status(Response),
  Body = busbudcc_test_helper:response_body(Response),
  #{<<"message">> := <<"Not found.">>} = jsx:decode(Body, [return_maps]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
