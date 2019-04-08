-module(busbudcc_404_handler).

-behaviour(cowboy_handler).

%% cowboy_handler callbacks
-export([init/2]).

%%%===================================================================
%%% cowboy_handler callbacks
%%%===================================================================

-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
init(Req, State) ->
  busbudcc_webutils:not_found(Req, State).

%%%===================================================================
%%% Internal functions
%%%===================================================================
