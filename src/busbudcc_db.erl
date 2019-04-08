-module(busbudcc_db).

%% API
-export([create_connection/0,
         close_connection/1,
         select_query/3,
         begin_transaction/1,
         end_transaction/1,
         abort_transaction/1]).

%% Macros
-define(TIMEOUT, 4000).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec create_connection() -> epgsql:connection().
create_connection() ->
  {ok, Host} = application:get_env(busbudcc, database_host),
  {ok, User} = application:get_env(busbudcc, database_user),
  {ok, Password} = application:get_env(busbudcc, database_password),
  {ok, DatabaseName} = application:get_env(busbudcc, database_name),
  {ok, Connection} = epgsql:connect(binary_to_list(Host),
                                    User,
                                    Password,
                                    [{database, DatabaseName},
                                     {timeout, ?TIMEOUT}]),
  Connection.

-spec close_connection(epgsql:connection()) -> ok.
close_connection(Connection) ->
  epgsql:close(Connection).

-spec select_query(epgsql:connection(), string(), []) -> [map()].
select_query(Connection, Query, Params) ->
  {ok, Columns, Rows} = epgsql:equery(Connection, Query, Params),
  [row_to_map(Columns, Row) || Row <- Rows].

-spec begin_transaction(epgsql:connection()) -> ok.
begin_transaction(Connection) ->
  {ok, _, _} = epgsql:squery(Connection, "BEGIN"),
  ok.

-spec end_transaction(epgsql:connection()) -> ok.
end_transaction(Connection) ->
  {ok, _, _} = epgsql:squery(Connection, "COMMIT"),
  ok.

-spec abort_transaction(epgsql:connection()) -> ok.
abort_transaction(Connection) ->
  {ok, _, _} = epgsql:squery(Connection, "ROLLBACK"),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

row_to_map(Columns, Row) ->
  maps:from_list(lists:zip([binary_to_atom(Name, utf8) ||
                             {column, Name, _, _, _, _, _} <- Columns],
                           tuple_to_list(Row))).
