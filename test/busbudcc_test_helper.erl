-module(busbudcc_test_helper).

%% Macros
-define(API_URL, "http://localhost").

%% API
-export([set_env_vars/0,
         request/3,
         response_body/1,
         response_status/1,
         response_content_type/1]).

%%%===================================================================
%%% API
%%%===================================================================

set_env_vars() ->
  [application:set_env(busbudcc, Name, Value) || {Name, Value} <- ct:get_config(busbudcc)].

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

response_body({_, _, Body}) ->
  list_to_binary(Body).

response_status({{_, Status, _}, _, _}) ->
  Status.

response_content_type({_, Headers, _}) ->
  proplists:get_value("content-type", Headers).

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_url(suggestions) ->
  <<"/suggestions">>;
make_url(PathOpts) when is_list(PathOpts) ->
  PathOpts.

format(Format, Data) ->
  lists:flatten(io_lib:format(Format, Data)).
