<?php

namespace App\Tests\Service\Suggestions\MockBuilder;

use Symfony\Component\HttpFoundation\ParameterBag;
use Symfony\Component\HttpFoundation\Request;

class RequestBuilder
{
    /** @var Request */
    protected $request;

    protected function buildBaseRequest(): Request
    {
        $request = new Request();

        $request->setMethod('GET');

        $request->query->set('q', 'Londo');

        $this->request = $request;

        return $request;
    }

    public function buildValidRequest(): Request
    {
        $this->buildBaseRequest();

        return $this->request;
    }

    public function buildValidRequestForParameters(): Request
    {
        $this->buildBaseRequest();

        $this->request->query = new ParameterBag();

        return $this->request;
    }
}