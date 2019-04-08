-module(busbudcc_webutils).

%% API
-export([not_found/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec not_found(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
not_found(Req, State) ->
  Req1 = cowboy_req:reply(404,
                          #{},
                          jsx:encode(#{<<"message">> => <<"Not found.">>}),
                          Req),
  {ok, Req1, State}.
