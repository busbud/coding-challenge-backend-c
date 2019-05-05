-module(busbudcc_db_pool_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([select_query/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% poolboy callbacks
-export([start_link/1]).

%% Macros
-define(TIMEOUT, 4000).

-record(state, {conn :: epgsql:connection()}).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec select_query(pid(), string(), []) -> [map()].
select_query(Worker, Query, Params) ->
  gen_server:call(Worker, {select_query, Query, Params}).

%%%===================================================================
%%% poolboy callbacks
%%%===================================================================

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, Host} = application:get_env(busbudcc, database_host),
  {ok, User} = application:get_env(busbudcc, database_user),
  {ok, Password} = application:get_env(busbudcc, database_password),
  {ok, DatabaseName} = application:get_env(busbudcc, database_name),
  {ok, Conn} = epgsql:connect(binary_to_list(Host),
                              binary_to_list(User),
                              binary_to_list(Password),
                              [{database, binary_to_list(DatabaseName)},
                               {timeout, ?TIMEOUT}]),
  {ok, #state{conn = Conn}}.

handle_call({select_query, Query, Params}, _From, State = #state{conn = Conn}) ->
  {ok, Columns, Rows} = epgsql:equery(Conn, Query, Params),
  {reply, [row_to_map(Columns, Row) || Row <- Rows], State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
  epgsql:close(Conn).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

row_to_map(Columns, Row) ->
  maps:from_list(lists:zip([binary_to_atom(Name, utf8) ||
                             {column, Name, _, _, _, _, _} <- Columns],
                           tuple_to_list(Row))).
