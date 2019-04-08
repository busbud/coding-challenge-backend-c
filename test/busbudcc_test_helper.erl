-module(busbudcc_test_helper).

%% API
-export([set_env_vars/0]).

%%%===================================================================
%%% API
%%%===================================================================

set_env_vars() ->
  [application:set_env(busbudcc, Name, Value) || {Name, Value} <- ct:get_config(busbudcc)].
