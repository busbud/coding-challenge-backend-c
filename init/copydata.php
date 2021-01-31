<?php
$credentials = getenv("DATABASE_URL") ?: 'postgres://php_challenge:php_challenge@localhost:5432/php_challenge';
try {
	ob_start();
	$connection = pg_connect($credentials);
	ob_get_clean();
	if(!$connection)
	{
		throw new RuntimeException('Connection Error!');
	}
} catch (\Exception $exception) {
	throw $exception;
}
$pg_query_params = function(string $sql, ?array $params = [])use($connection) {
	$result = pg_query_params($connection, $sql, $params);
	$error = pg_last_error($connection);
	if($error)
	{
		print_r($error);
		return null;
	}
	$array = pg_fetch_all($result, PGSQL_ASSOC);
	return $array;
};

$mapping = [
$mappings = [
	'id' => [
		'geonameid',
		function($value) {
			return intval($value);
		}
	],
	'name' => [
		'name',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'ascii' => [
		'asciiname',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'alt_name' => [
		'alternatenames',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'lat' => [
		'latitude',
		function($value) {
			return floatval($value);
		}
	],
	'long' => [
		'longitude',
		function($value) {
			return floatval($value);
		}
	],
	'feat_class' => [
		'feature_class',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'feat_code' => [
		'feature_code',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'country' => [
		'country_code',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'cc2' => [
		'cc2',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'admin1' => [
		'admin1_code',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'admin2' => [
		'admin2_code',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'admin3' => [
		'admin3_code',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'admin4' => [
		'admin4_code',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'population' => [
		'population',
		function($value) {
			return intval($value);
		}
	],
	'elevation' => [
		'elevation',
		function($value) {
			return intval($value);
		}
	],
	'dem' => [
		'dem',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'tz' => [
		'timezone',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
	'modified_at' => [
		'modification_date',
		function($value) {
			return filter_var($value, FILTER_SANITIZE_STRING);
		}
	],
];

$pg_query_params('DROP TABLE IF EXISTS "public"."cities";');
$pg_query_params('CREATE TABLE IF NOT EXISTS "public"."cities" (
	"geonameid" integer NOT NULL,
	"name" character varying(200) NOT NULL,
	"asciiname" character varying(200) NOT NULL,
	"alternatenames" character varying(5000) NOT NULL,
	"latitude" numeric(9,6) NOT NULL,
	"longitude" numeric(9,6) NOT NULL,
	"feature_class" character varying(1) NOT NULL,
	"feature_code" character varying(10) NOT NULL,
	"country_code" character varying(2) NOT NULL,
	"cc2" character varying(60) NOT NULL,
	"admin1_code" character varying(20) NOT NULL,
	"admin2_code" character varying(80) NOT NULL,
	"admin3_code" character varying(20) NOT NULL,
	"admin4_code" character varying(20) NOT NULL,
	"population" bigint NOT NULL,
	"elevation" integer NOT NULL,
	"dem" character varying(20) NOT NULL,
	"timezone" character varying(40) NOT NULL,
	"modification_date" date NOT NULL
	);
');
$columns = $pg_query_params('SELECT
		"table_name",
		"column_name",
		"data_type"
	 FROM
		information_schema.columns
	 WHERE
		table_name = $1;
', ['cities']);
if(count($columns) !== count($mappings))
{
	throw new RuntimeException('Table Structure Error!');
}

$records = array_filter(file(__DIR__ . '/../data/cities_canada-usa.tsv', FILE_SKIP_EMPTY_LINES | FILE_IGNORE_NEW_LINES));
$columns = explode("\t", array_shift($records));
foreach($records as $record){
	$params = [];
	$values = array_combine($columns, explode("\t", $record));
	foreach($values as $key => $value){
		if(!array_key_exists($key, $mappings))
		{
			throw new RuntimeException("Data Mapping Error for $key!");
		}
		$params[$key] = $mappings[$key][1]($value);
	}
	$pg_query_params('INSERT INTO "public"."cities" (
			"geonameid",
			"name",
			"asciiname",
			"alternatenames",
			"latitude",
			"longitude",
			"feature_class",
			"feature_code",
			"country_code",
			"cc2",
			"admin1_code",
			"admin2_code",
			"admin3_code",
			"admin4_code",
			"population",
			"elevation",
			"dem",
			"timezone",
			"modification_date"
		) VALUES (
			$1,
			$2,
			$3,
			$4,
			$5,
			$6,
			$7,
			$8,
			$9,
			$10,
			$11,
			$12,
			$13,
			$14,
			$15,
			$16,
			$17,
			$18,
			$19
		);
	',
	$params);
}

// Count data to verify copy operation success
$count = $pg_query_params('SELECT COUNT(*) AS "count" FROM "public"."cities"');
if((int) $count[0]['count'] === count($records))
{
	echo "Data copied successfully.\n";
}
else
{
	echo "Some data could not be copied!\n";
}