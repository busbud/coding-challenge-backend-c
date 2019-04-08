-module(busbudcc_suggestions_handler).

%% cowboy_http_handler callbacks
-export([init/2,
         terminate/3]).

-record(state, {connection :: epgsql:connection()}).

%%%===================================================================
%%% cowboy_http_handler callbacks
%%%===================================================================

init(Req, _Opts) ->
  Method = busbudcc_webutils:method(Req),
  Conn = busbudcc_db:create_connection(),
  Res = handle_req(Method, Req, #state{connection = Conn}),
  ok = busbudcc_db:close_connection(Conn),
  Res.

terminate(_Reason, _Req, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_req(<<"GET">>, Req, State) ->
  get_suggestions(Req, State);
handle_req(_Method, Req, State) ->
  busbudcc_webutils:not_found(Req, State).

get_suggestions(Req, State = #state{connection = Conn}) ->
  Params = busbudcc_webutils:query_params(Req),
  case busbudcc_cities_entity:suggest(Conn, Params) of
    {ok, Suggestions} ->
      busbudcc_webutils:success(Req, State, 200, #{suggestions => Suggestions});
    {error, Reason} ->
      busbudcc_webutils:invalid_content(Req, State, Reason)
  end.
