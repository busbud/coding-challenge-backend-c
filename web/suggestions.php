<?php
require_once __DIR__ . '/../lib/pgsql.php';
$mappings = [
	'q' => [
		'search',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'latitude' => [
		'latitude',
		function($value) {
			return floatval($value);
		}
	],
	'longitude' => [
		'longitude',
		function($value) {
			return floatval($value);
		}
	],
];
$params = [];
foreach($mappings as $field => $mapping){
	if(array_key_exists($field, $_GET))
	{
		$params[$mapping[0]] = $mapping[1]($_GET[$field]);
		continue;
	}
	break;
}
if(!array_key_exists('search', $params))
{
	http_response_code(400);
	exit("Search term must be suppiled through 'q' query parameter!");
}
$coordinates = array_filter([
	'latitude' => $params['latitude'] ?? null,
	'longitude' => $params['longitude'] ?? null,
]);
$where = "
	(name ILIKE $2 OR asciiname ILIKE $2 OR alternatenames LIKE $2)
	AND population > 5000
	AND (country_code = 'US' OR country_code = 'CA')
	";
$distance = 3; //In degress one degress approximitly 111km
foreach($coordinates as $axis => $coordinate){
	$where .= "\n" . ' AND ('. ($coordinate - $distance) . ' < ' . $axis . ' AND ' . $axis . ' < ' . ($coordinate + $distance) . ')';
}
$sql = "
	SELECT
		CONCAT(name, ', ', admin1_code, ', ', country_code) AS name,
		latitude,
		longitude,
		CASE levenshtein(name, $1::varchar(256))
			WHEN 0 THEN 1
			ELSE  ROUND(1.0 / levenshtein(name, $1::varchar(256)),2)
		END AS score
	FROM public.cities
	WHERE $where
	ORDER BY score DESC, name DESC
	;";
$matches = $pg_query_params($sql, [$params['search'], "%{$params['search']}%"]);
if(!$matches){
	http_response_code(404);
}
header('Content-Type: application/json; charset=UTF-8');
echo json_encode(['suggestions' => $matches ?: []]);
