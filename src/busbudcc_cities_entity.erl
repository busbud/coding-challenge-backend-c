-module(busbudcc_cities_entity).

%% API
-export([suggest/2]).

%% Macros
-define(MAX_CITIES, 25).
-define(WORD_WEIGHT, 50).
-define(MAX_WORD_DISTANCE, 10).
-define(LOCATION_WEIGHT, 50).
-define(MAX_LOCATION_DISTANCE, 1000000).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec suggest(epgsql:connection(), map()) -> {ok, [map()]} |
                                             {error, binary()}.
%% SCORE = WORD_WEIGHT     - MAX(WORD_WEIGHT     * WORD_DISTANCE, 0)     / MAX_WORD_DISTANCE +
%%         LOCATION_WEIGHT - MAX(LOCATION_WEIGHT * LOCATION_DISTANCE, 0) / MAX_LOCATION_DISTANCE
suggest(Conn, #{<<"q">> := SearchText,
                <<"latitude">> := Lat,
                <<"longitude">> := Lon}) when is_binary(SearchText) andalso
                                              is_binary(Lat) andalso
                                              is_binary(Lon) ->
  SearchTextStr = binary_to_list(SearchText),
  case parse_location(Lon, Lat) of
    {ok, LonFloat, LatFloat} ->
      Cities = busbudcc_db:select_query(
                 Conn,
                 "SELECT * "
                 "FROM ("
                 "  SELECT cities.name, cities.country, "
                 "    cities.latitude, cities.longitude, "
                 "    $1 - $2 * LEVENSHTEIN($3, LOWER(cities.name), 1, 10, 10) / $4 + "
                 "    GREATEST($5 - $6 * ST_DISTANCE(cities.location, ST_POINT($7, $8)) / $9, 0)"
                 "    AS score "
                 "  FROM cities "
                 "  WHERE cities.name ILIKE $10 "
                 "  ORDER BY score DESC "
                 "  LIMIT $11"
                 ") AS inner_cities "
                 "WHERE score > 0",
                 [?WORD_WEIGHT, ?WORD_WEIGHT, string:to_lower(SearchTextStr), ?MAX_WORD_DISTANCE,
                  ?LOCATION_WEIGHT, ?LOCATION_WEIGHT, LonFloat, LatFloat, ?MAX_LOCATION_DISTANCE,
                  sanitize_search_text(SearchText),
                  ?MAX_CITIES]),
      {ok, serialize_cities(Cities)};
    error -> {error, <<"Invalid location format.">>}
  end;
suggest(Conn, #{<<"q">> := SearchText}) when is_binary(SearchText) ->
  SearchTextStr = binary_to_list(SearchText),
  Cities = busbudcc_db:select_query(
             Conn,
             "SELECT * "
             "FROM ("
             "  SELECT cities.name, cities.country, "
             "    cities.latitude, cities.longitude, "
             "    100 - 100 * LEVENSHTEIN($1, LOWER(cities.name), 1, 10, 10) / $2 AS score "
             "  FROM cities "
             "  WHERE cities.name ILIKE $3 "
             "  ORDER BY score DESC "
             "  LIMIT $4"
             ") AS inner_cities "
             "WHERE score > 0",
             [string:to_lower(SearchTextStr), ?MAX_WORD_DISTANCE,
              sanitize_search_text(SearchTextStr), ?MAX_CITIES]),
  {ok, serialize_cities(Cities)};
suggest(_Conn, _SearchParams) ->
  {error, <<"Invalid params format.">>}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_location(Lon, Lat) ->
  try
    {ok, binary_to_float(Lon), binary_to_float(Lat)}
  catch
    _C:_E -> error
  end.

serialize_cities(Cities) ->
  [serialize_city(C) || C <- Cities].

serialize_city(#{name := Name,
                 country := Country,
                 score := Score,
                 latitude := Lat,
                 longitude := Lon}) ->
  #{name => serialize_name(Name, Country),
    score => round(Score) / 100,
    latitude => Lat,
    longitude => Lon}.

serialize_name(Name, <<"US">>) ->
  list_to_binary(lists:flatten(io_lib:format("~s, USA", [Name])));
serialize_name(Name, <<"CA">>) ->
  list_to_binary(lists:flatten(io_lib:format("~s, Canada", [Name])));
serialize_name(Name, Country) ->
  list_to_binary(lists:flatten(io_lib:format("~s, ~s", [Name, Country]))).

sanitize_search_text(SearchText) ->
  string:replace(SearchText, "%", "\\%") ++ "%".
