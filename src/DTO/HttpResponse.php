<?php

namespace Src\DTO;

class HttpResponse
{
    public function unprocessableEntityResponse()
    {
        header('HTTP/1.1 422 Unprocessable Entity');
        echo  json_encode([
            'error' => 'Invalid input'
        ]);

        exit();
    }

    public function notFoundResponse($body = array())
    {
        header('HTTP/1.1 404 Not Found');
        echo  json_encode($body, true);
        exit();
    }

    public function failResponse($body = array())
    {
        header('HTTP/1.1 500 Internal Server Error');
        echo  json_encode($body, true);
        exit();
    }

    public function successResponse($body = array())
    {
        header('HTTP/1.1 200 OK');
        echo  json_encode($body, true);
        exit();
    }
}