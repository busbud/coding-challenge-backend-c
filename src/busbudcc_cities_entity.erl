-module(busbudcc_cities_entity).

%% API
-export([suggest/1,
         suggest_cached/1]).

%% Macros
-define(MAX_CITIES, 25).
-define(WORD_WEIGHT, 50).
-define(MAX_WORD_DISTANCE, 10).
-define(LOCATION_WEIGHT, 50).
-define(MAX_LOCATION_DISTANCE, 1000000).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec suggest_cached(map()) -> {ok, [map()]} | {error, binary()}.
suggest_cached(#{<<"q">> := SearchText,
                 <<"latitude">> := Lat,
                 <<"longitude">> := Lon}) when is_binary(SearchText) andalso
                                               is_binary(Lat) andalso
                                               is_binary(Lon) ->
  SearchTextStr = binary_to_list(SearchText),
  case parse_location(Lon, Lat) of
    {ok, LonFloat, LatFloat} ->
      {ok, busbudcc_suggestions_cache:suggest({located, SearchTextStr, LonFloat, LatFloat})};
    error -> {error, <<"Invalid location format.">>}
  end;
suggest_cached(#{<<"q">> := SearchText}) when is_binary(SearchText) ->
  SearchTextStr = binary_to_list(SearchText),
  {ok, busbudcc_suggestions_cache:suggest({unlocated, SearchTextStr})};
suggest_cached(_SearchParams) ->
  {error, <<"Invalid params format.">>}.

-spec suggest({located, string(), float(), float()} |
              {unlocated, string()}) -> [map()].
%% SCORE = WORD_WEIGHT     - MAX(WORD_WEIGHT     * WORD_DISTANCE, 0)     / MAX_WORD_DISTANCE +
%%         LOCATION_WEIGHT - MAX(LOCATION_WEIGHT * LOCATION_DISTANCE, 0) / MAX_LOCATION_DISTANCE
suggest({located, SearchTextStr, LonFloat, LatFloat}) ->
  Cities = busbudcc_db:select_query(
             "SELECT * "
             "FROM ("
             "  SELECT cities.name, cities.state, cities.country, "
             "    cities.latitude, cities.longitude, "
             "    $1 - $2 * LEVENSHTEIN($3, LOWER(cities.name), 1, 10, 10) / $4 + "
             "    GREATEST($5 - $6 * ST_DISTANCE(cities.location, ST_POINT($7, $8)) / $9, 0)"
             "    AS score "
             "  FROM cities "
             "  WHERE LOWER(cities.name) LIKE $10 "
             "  ORDER BY score DESC "
             "  LIMIT $11"
             ") AS inner_cities "
             "WHERE score > 0",
             [?WORD_WEIGHT, ?WORD_WEIGHT, string:to_lower(SearchTextStr), ?MAX_WORD_DISTANCE,
              ?LOCATION_WEIGHT, ?LOCATION_WEIGHT, LonFloat, LatFloat, ?MAX_LOCATION_DISTANCE,
              sanitize_search_text(SearchTextStr),
              ?MAX_CITIES]),
  serialize_cities(Cities);
suggest({unlocated, SearchTextStr}) ->
  Cities = busbudcc_db:select_query(
             "SELECT * "
             "FROM ("
             "  SELECT cities.name, cities.state, cities.country, "
             "    cities.latitude, cities.longitude, "
             "    100 - 100 * LEVENSHTEIN($1, LOWER(cities.name), 1, 10, 10) / $2 AS score "
             "  FROM cities "
             "  WHERE LOWER(cities.name) LIKE $3 "
             "  ORDER BY score DESC "
             "  LIMIT $4"
             ") AS inner_cities "
             "WHERE score > 0",
             [string:to_lower(SearchTextStr), ?MAX_WORD_DISTANCE,
              sanitize_search_text(SearchTextStr), ?MAX_CITIES]),
  serialize_cities(Cities).

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
                 state := State,
                 country := Country,
                 score := Score,
                 latitude := Lat,
                 longitude := Lon}) ->
  #{name => serialize_name(Name, State, Country),
    score => round(Score) / 100,
    latitude => Lat,
    longitude => Lon}.

serialize_name(Name, State, <<"US">>) ->
  list_to_binary(lists:flatten(io_lib:format("~s, ~s, USA", [Name, State])));
serialize_name(Name, State, <<"CA">>) ->
  list_to_binary(lists:flatten(io_lib:format("~s, ~s, Canada", [Name, State])));
serialize_name(Name, State, Country) ->
  list_to_binary(lists:flatten(io_lib:format("~s, ~s, ~s", [Name, State, Country]))).

sanitize_search_text(SearchText) ->
  string:to_lower(lists:flatten(string:replace(SearchText, "%", "\\%"))) ++ "%".
