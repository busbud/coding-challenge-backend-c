<?php

namespace Test\unit\Http\Controllers;

use App\Http\Controllers\AutoCompleteController;
use Config;
use Mockery;

class AutoCompleteControllerTest extends \Codeception\Test\Unit
{

    protected $tester;
    protected $autoCompleteController;

    public function _after()
    {
        Mockery::close();
    }

    public function _before()
    {
        $this->autoCompleteController = Mockery::mock(AutoCompleteController::class)->makepartial();
    }

    public function test_should_give_suggestions_on_elastic_search_with_query_parameters()
    {
        $query = Mockery::mock('alias:SuggestionRequest')
            ->shouldReceive('q')
            ->once()
            ->andReturn('Londo')
            ->shouldReceive('latitude')
            ->once()
            ->andReturn(null)
            ->shouldReceive('longitude')
            ->once()
            ->andReturn(null);

        Config::shouldReceive('get')
            ->once()
            ->with('services.search.enabled')
            ->andReturn(true);

        $this->tester->makeFacadeClass(Config::class);

        $suggestionResult = [
            [
                'suggestions' => [
                    [
                        "name" => "London, ON, Canada",
                        "latitude" => "42.98339",
                        "longitude" => "-81.23304",
                        "score" => 0.9
                    ],
                    [
                        "name" => "London, OH, USA",
                        "latitude" => "39.88645",
                        "longitude" => "-83.44825",
                        "score" => 0.5
                    ],
                    [
                        "name" => "London, KY, USA",
                        "latitude" => "37.12898",
                        "longitude" => "-84.08326",
                        "score" => 0.5
                    ],
                    [
                        "name" => "Londontowne, MD, USA",
                        "latitude" => "38.93345",
                        "longitude" => "-76.54941",
                        "score" => 0.3
                    ]
                ]
            ]
        ];

        Mockery::mock('alias:ElasticSearchCitiesRepository')
            ->shouldReceive('search')
            ->once()
            ->with('Londo', null, null)
            ->andReturn($suggestionResult);

        $this->autoCompleteController->shouldReceive('response')
            ->once()
            ->with($query)
            ->andReturn($query, 200);

        $response = $this->autoCompleteController->suggestions();

        $this->assertNotNull($response);
        $this->assertEquals(200, $response->getStatusCode());
        $this->assertSame($suggestionResult, $response);
    }

    public function test_should_give_suggestions_on_elastic_search_with_query_and_latitude_parameters()
    {
        $query = Mockery::mock('alias:SuggestionRequest')
            ->shouldReceive('q')
            ->once()
            ->andReturn('Londo')
            ->shouldReceive('latitude')
            ->once()
            ->andReturn(38.93345)
            ->shouldReceive('longitude')
            ->once()
            ->andReturn(-76.54941);

        Config::shouldReceive('get')
            ->once()
            ->with('services.search.enabled')
            ->andReturn(true);

        $this->tester->makeFacadeClass(Config::class);

        $suggestionResult = [
            [
                'suggestions' => [
                    [
                        "name" => "Londontowne, MD, USA",
                        "latitude" => "38.93345",
                        "longitude" => "-76.54941",
                        "score" => 0.3
                    ]
                ]
            ]
        ];

        Mockery::mock('alias:ElasticSearchCitiesRepository')
            ->shouldReceive('search')
            ->once()
            ->with('Londo', null, null)
            ->andReturn($suggestionResult);

        $this->autoCompleteController->shouldReceive('response')
            ->once()
            ->with($query)
            ->andReturn($query, 200);

        $response = $this->autoCompleteController->suggestions();

        $this->assertNotNull($response);
        $this->assertEquals(200, $response->getStatusCode());
        $this->assertSame($suggestionResult, $response);
    }

    public function test_should_give_suggestions_on_elastic_search_with_query_and_latitude_and_longitude_parameters()
    {
        $query = Mockery::mock('alias:SuggestionRequest')
            ->shouldReceive('q')
            ->once()
            ->andReturn('Londo')
            ->shouldReceive('latitude')
            ->once()
            ->andReturn(42.98339)
            ->shouldReceive('longitude')
            ->once()
            ->andReturn(null);

        Config::shouldReceive('get')
            ->once()
            ->with('services.search.enabled')
            ->andReturn(true);

        $this->tester->makeFacadeClass(Config::class);

        $suggestionResult = [
            [
                'suggestions' => [
                    [
                        "name" => "London, ON, Canada",
                        "latitude" => "42.98339",
                        "longitude" => "-81.23304",
                        "score" => 0.9
                    ]
                ]
            ]
        ];

        Mockery::mock('alias:ElasticSearchCitiesRepository')
            ->shouldReceive('search')
            ->once()
            ->with('Londo', null, null)
            ->andReturn($suggestionResult);

        $this->autoCompleteController->shouldReceive('response')
            ->once()
            ->with($query)
            ->andReturn($query, 200);

        $response = $this->autoCompleteController->suggestions();

        $this->assertNotNull($response);
        $this->assertEquals(200, $response->getStatusCode());
        $this->assertSame($suggestionResult, $response);
    }

    public function test_should_give_suggestions_on_database_with_query_parameters()
    {
        $query = Mockery::mock('alias:SuggestionRequest')
            ->shouldReceive('q')
            ->once()
            ->andReturn('Londo')
            ->shouldReceive('latitude')
            ->once()
            ->andReturn(null)
            ->shouldReceive('longitude')
            ->once()
            ->andReturn(null);

        Config::shouldReceive('get')
            ->once()
            ->with('services.search.enabled')
            ->andReturn(false);

        $this->tester->makeFacadeClass(Config::class);

        $suggestionResult = [
            [
                'suggestions' => [
                    [
                        "name" => "London, ON, Canada",
                        "latitude" => "42.98339",
                        "longitude" => "-81.23304",
                        "score" => 0.9
                    ],
                    [
                        "name" => "London, OH, USA",
                        "latitude" => "39.88645",
                        "longitude" => "-83.44825",
                        "score" => 0.5
                    ],
                    [
                        "name" => "London, KY, USA",
                        "latitude" => "37.12898",
                        "longitude" => "-84.08326",
                        "score" => 0.5
                    ],
                    [
                        "name" => "Londontowne, MD, USA",
                        "latitude" => "38.93345",
                        "longitude" => "-76.54941",
                        "score" => 0.3
                    ]
                ]
            ]
        ];

        Mockery::mock('alias:CityRepository')
            ->shouldReceive('search')
            ->once()
            ->with('Londo', null, null)
            ->andReturn($suggestionResult);

        $this->autoCompleteController->shouldReceive('response')
            ->once()
            ->with($query)
            ->andReturn($query, 200);

        $response = $this->autoCompleteController->suggestions();

        $this->assertNotNull($response);
        $this->assertEquals(200, $response->getStatusCode());
        $this->assertSame($suggestionResult, $response);
    }

    public function test_should_give_suggestions_on_database_with_query_and_latitude_and_longitude_parameters()
    {
        $query = Mockery::mock('alias:SuggestionRequest')
            ->shouldReceive('q')
            ->once()
            ->andReturn('Londo')
            ->shouldReceive('latitude')
            ->once()
            ->andReturn(38.93345)
            ->shouldReceive('longitude')
            ->once()
            ->andReturn(-76.54941);

        Config::shouldReceive('get')
            ->once()
            ->with('services.search.enabled')
            ->andReturn(false);

        $this->tester->makeFacadeClass(Config::class);

        $suggestionResult = [
            [
                'suggestions' => [
                    [
                        "name" => "Londontowne, MD, USA",
                        "latitude" => "38.93345",
                        "longitude" => "-76.54941",
                        "score" => 0.3
                    ]
                ]
            ]
        ];

        Mockery::mock('alias:CityRepository')
            ->shouldReceive('search')
            ->once()
            ->with('Londo', null, null)
            ->andReturn($suggestionResult);

        $this->autoCompleteController->shouldReceive('response')
            ->once()
            ->with($query)
            ->andReturn($query, 200);

        $response = $this->autoCompleteController->suggestions();

        $this->assertNotNull($response);
        $this->assertEquals(200, $response->getStatusCode());
        $this->assertSame($suggestionResult, $response);
    }

    public function test_should_give_suggestions_on_database_with_query_and_latitude_parameters()
    {
        $query = Mockery::mock('alias:SuggestionRequest')
            ->shouldReceive('q')
            ->once()
            ->andReturn('Londo')
            ->shouldReceive('latitude')
            ->once()
            ->andReturn(42.98339)
            ->shouldReceive('longitude')
            ->once()
            ->andReturn(null);

        Config::shouldReceive('get')
            ->once()
            ->with('services.search.enabled')
            ->andReturn(false);

        $this->tester->makeFacadeClass(Config::class);

        $suggestionResult = [
            [
                'suggestions' => [
                    [
                        "name" => "London, ON, Canada",
                        "latitude" => "42.98339",
                        "longitude" => "-81.23304",
                        "score" => 0.9
                    ]
                ]
            ]
        ];

        Mockery::mock('alias:CityRepository')
            ->shouldReceive('search')
            ->once()
            ->with('Londo', null, null)
            ->andReturn($suggestionResult);

        $this->autoCompleteController->shouldReceive('response')
            ->once()
            ->with($query)
            ->andReturn($query, 200);

        $response = $this->autoCompleteController->suggestions();

        $this->assertNull($response['suggestions']);
        $this->assertEquals(200, $response->getStatusCode());
        $this->assertSame($suggestionResult, $response);
    }

    public function test_should_give_empty_suggestions_on_database_with_invalid_query_parameter()
    {
        $query = Mockery::mock('alias:SuggestionRequest')
            ->shouldReceive('q')
            ->once()
            ->andReturn('asdlsaldld')
            ->shouldReceive('latitude')
            ->once()
            ->andReturn(null)
            ->shouldReceive('longitude')
            ->once()
            ->andReturn(null);

        Config::shouldReceive('get')
            ->once()
            ->with('services.search.enabled')
            ->andReturn(false);

        $this->tester->makeFacadeClass(Config::class);

        $suggestionResult = [
            [
                'suggestions' => [
                    [
                    ]
                ]
            ]
        ];

        Mockery::mock('alias:CityRepository')
            ->shouldReceive('search')
            ->once()
            ->with('asdlsaldld', null, null)
            ->andReturn($suggestionResult);

        $this->autoCompleteController->shouldReceive('response')
            ->once()
            ->with($query)
            ->andReturn($query, 200);

        $response = $this->autoCompleteController->suggestions();

        $this->assertNull($response['suggestions']);
        $this->assertEquals(200, $response->getStatusCode());
        $this->assertSame($suggestionResult, $response);
    }

    public function test_should_give_empty_suggestions_on_elastic_search_with_invalid_query_parameters()
    {
        $query = Mockery::mock('alias:SuggestionRequest')
            ->shouldReceive('q')
            ->once()
            ->andReturn('sadlldlldld')
            ->shouldReceive('latitude')
            ->once()
            ->andReturn(null)
            ->shouldReceive('longitude')
            ->once()
            ->andReturn(null);

        Config::shouldReceive('get')
            ->once()
            ->with('services.search.enabled')
            ->andReturn(true);

        $this->tester->makeFacadeClass(Config::class);

        $suggestionResult = [
            [
                'suggestions' => []
            ]
        ];

        Mockery::mock('alias:ElasticSearchCitiesRepository')
            ->shouldReceive('search')
            ->once()
            ->with('sadlldlldld', null, null)
            ->andReturn($suggestionResult);

        $this->autoCompleteController->shouldReceive('response')
            ->once()
            ->with($query)
            ->andReturn($query, 200);

        $response = $this->autoCompleteController->suggestions();

        $this->assertNull($response['suggestions']);
        $this->assertEquals(200, $response->getStatusCode());
        $this->assertSame($suggestionResult, $response);
    }

    public function test_should_give_empty_suggestions_on_elastic_search_with_empty_query_parameters()
    {
        $query = Mockery::mock('alias:SuggestionRequest')
            ->shouldReceive('q')
            ->once()
            ->andReturn(null)
            ->shouldReceive('latitude')
            ->once()
            ->andReturn(null)
            ->shouldReceive('longitude')
            ->once()
            ->andReturn(null);

        Config::shouldReceive('get')
            ->once()
            ->with('services.search.enabled')
            ->andReturn(true);

        $this->tester->makeFacadeClass(Config::class);

        $suggestionResult = [
            [
                'suggestions' => []
            ]
        ];

        Mockery::mock('alias:ElasticSearchCitiesRepository')
            ->shouldReceive('search')
            ->once()
            ->with(null, null, null)
            ->andReturn($suggestionResult);

        $this->autoCompleteController->shouldReceive('response')
            ->once()
            ->with($query)
            ->andReturn($query, 200);

        $response = $this->autoCompleteController->suggestions();

        $this->assertNull($response['suggestions']);
        $this->assertEquals(200, $response->getStatusCode());
        $this->assertSame($suggestionResult, $response);
    }

}
