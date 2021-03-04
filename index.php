<?php

include(__DIR__ . '/bootstrap.php');

use Src\Controller\SuggestionController;
use Src\System\DatabaseConnector;
use Src\Library\Utilities;
use Src\DTO\HttpResponse;

Utilities::setApiHeader();

if (Utilities::getPath() !== 'suggestions') {
    $response = new HttpResponse();
    $response->notFoundResponse();
}

$query = Utilities::getParams();
try {
    $dbConnection = (new DatabaseConnector())->getConnection();
    $controller = new SuggestionController($dbConnection, $_SERVER["REQUEST_METHOD"]);
    $controller->processRequest($query);
} catch (Exception $e) {
    echo $e->getMessage();
    $response = new HttpResponse();
    $response->failResponse();
}

