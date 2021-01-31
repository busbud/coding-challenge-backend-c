<?php
// routing for suggestions endpoint
// IMPORTANT : Only for development.
//             Do not deploy to production.
//             see README.md inside ./web folder

$endpoints = [
	'index'
];
if(preg_match('#^/(?:' . implode('|', $endpoints) . ')$#', $_SERVER["REQUEST_URI"]))
{
	header('Content-Type: text/html; charset=UTF-8');
	include __DIR__ . DIRECTORY_SEPARATOR . 'web' . $_SERVER["REQUEST_URI"] . '.php';
}
else
{
	http_response_code(404);
	header('Content-Type: text/plain; charset=UTF-8');
	echo 'Not Found!';
}
?>