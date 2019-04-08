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
  Response = request(get, "/blah", #{}),
  404 = response_status(Response),
  #{<<"message">> := <<"Not found.">>} = jsx:decode(response_body(Response), [return_maps]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

request(get, PathOpts, QueryParams) ->
  Port = ct:get_config({busbudcc, webserver_port}),
  EncodedQueryParams = binary_to_list(cow_qs:qs(maps:to_list(QueryParams))),
  Url = format("~s:~B~s?~s", [?API_URL, Port, make_url(PathOpts), EncodedQueryParams]),
  {ok, Response} = httpc:request(get, {Url, []}, [], []),
  Response;
request(Method, PathOpts, Params) ->
  Port = ct:get_config({busbudcc, webserver_port}),
  EncodedParams = jsx:encode(Params),
  Url = format("~s:~B~s", [?API_URL, Port, make_url(PathOpts)]),
  Request = {Url, [], "application/json", EncodedParams},
  {ok, Response} = httpc:request(Method, Request, [], []),
  Response.

make_url(PathOpts) when is_list(PathOpts) ->
  PathOpts.

response_body({_, _, Body}) ->
  list_to_binary(Body).

response_status({{_, Status, _}, _, _}) ->
  Status.

format(Format, Data) ->
  lists:flatten(io_lib:format(Format, Data)).
