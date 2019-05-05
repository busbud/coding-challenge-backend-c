-module(busbudcc_webutils).

%% API
-export([success/4,
         not_found/2,
         invalid_content/3,
         method/1,
         query_params/1]).

-define(JSON_HEADERS, #{<<"Content-Type">> => <<"application/json">>}).

%%%===================================================================
%%% API
%%%===================================================================

-spec success(cowboy_req:req(), any(), integer(), term()) -> {ok, cowboy_req:req(), any()}.
success(Req, State, StatusCode, Content) ->
  Req1 = cowboy_req:reply(StatusCode, ?JSON_HEADERS, jsx:encode(Content), Req),
  {ok, Req1, State}.

-spec not_found(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
not_found(Req, State) ->
  Req1 = cowboy_req:reply(404,
                          ?JSON_HEADERS,
                          jsx:encode(#{<<"message">> => <<"Not found.">>}),
                          Req),
  {ok, Req1, State}.

-spec invalid_content(cowboy_req:req(), any(), binary()) -> {ok, cowboy_req:req(), any()}.
invalid_content(Req, State, Error) ->
  Req1 = cowboy_req:reply(422, ?JSON_HEADERS, jsx:encode(#{<<"error">> => Error}), Req),
  {ok, Req1, State}.

-spec method(cowboy_req:req()) -> binary().
method(Req) ->
  case cowboy_req:header(<<"access-control-request-method">>, Req) of
    undefined -> cowboy_req:method(Req);
    Method -> Method
  end.

-spec query_params(cowboy_req:req()) -> map().
query_params(Req) ->
  maps:from_list(cowboy_req:parse_qs(Req)).
