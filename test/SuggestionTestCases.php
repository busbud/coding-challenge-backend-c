<?php

require 'vendor/autoload.php';

use PHPUnit\Framework\TestCase;

class SuggestionTestCases extends TestCase
{
    protected $client;

    protected function setUp(): void
    {
        $this->client = new GuzzleHttp\Client([
            'base_uri' => 'http://localhost'
        ]);
    }

    public function testGet_InvalidParam_returns422()
    {
        $response = $this->client->get('/suggestions?ggg=hh',
            ['http_errors' => false]
        );

        $this->assertEquals(422, $response->getStatusCode());
    }

    public function testGet_SomeRandomCityInTheMiddleOfNowhere_returns404()
    {
        $response = $this->client->get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere',
            ['http_errors' => false]
        );

        $this->assertEquals(404, $response->getStatusCode());
    }

    public function testGet_SomeRandomCityInTheMiddleOfNowhere_returnsArrayWithSuggestionsKey()
    {
        $response = $this->client->get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere',
            ['http_errors' => false]
        );
        $data = json_decode($response->getBody(), true);
        $this->assertArrayHasKey('suggestions', $data);
    }

    public function testGet_SomeRandomCityInTheMiddleOfNowhere_returnsEmptyArray()
    {
        $response = $this->client->get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere',
            ['http_errors' => false]
        );

        $data = json_decode($response->getBody(), true);
        $this->assertEmpty($data['suggestions']);
    }

    public function testGet_ValidCity_returns200()
    {
        $response = $this->client->get('/suggestions?q=Montreal',
            ['http_errors' => false]
        );

        $this->assertEquals(200, $response->getStatusCode());
    }

    public function testGet_ValidCity_returnsArrayWithSuggestionsKey()
    {
        $response = $this->client->get('/suggestions?q=Montreal',
            ['http_errors' => false]
        );
        $data = json_decode($response->getBody(), true);
        $this->assertArrayHasKey('suggestions', $data);
    }

    public function testGet_ValidCity_returnsNotEmptyArray()
    {
        $response = $this->client->get('/suggestions?q=Montreal',
            ['http_errors' => false]
        );

        $data = json_decode($response->getBody(), true);
        !$this->assertNotEmpty($data['suggestions']);
    }

    public function testGet_ValidCity_returnsValidData()
    {
        $response = $this->client->get('/suggestions?q=Montreal',
            ['http_errors' => false]
        );

        $data = json_decode($response->getBody(), true);

        foreach ($data['suggestions'] as $item) {
            $this->assertArrayHasKey('latitude', $item);
            $this->assertArrayHasKey('longitude', $item);
            $this->assertArrayHasKey('score', $item);
        }
    }

    public function testGet_ValidCity_containsMatch()
    {
        $response = $this->client->get('/suggestions?q=Montreal',
            ['http_errors' => false]
        );

        $data = json_decode($response->getBody(), true);
        $found = false;

        foreach ($data['suggestions'] as $item) {
            if(strpos($item["name"], "Montréal") !== false){
                $found = true;
                break;
            }
        }

        $this->assertEquals(true,$found);
    }
}