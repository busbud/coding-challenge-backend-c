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
