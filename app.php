<?php
// routing for suggestions endpoint
// IMPORTANT : Only for development.
//             Do not deploy to production.
//             see README.md inside ./web folder

$endpoints = [
	'index',
	'suggestions',
];
$uri = explode('?', $_SERVER["REQUEST_URI"])[0] ?? '';
if(preg_match('#^/(?:' . implode('|', $endpoints) . ')$#', $uri))
{
	header('Content-Type: text/html; charset=UTF-8');
	include __DIR__ . DIRECTORY_SEPARATOR . 'web' . $uri . '.php';
}
else
{
	http_response_code(404);
	header('Content-Type: text/plain; charset=UTF-8');
	echo 'Not Found!';
}
?>