<?php

namespace Test\unit\Repositories;

use App\Repositories\ElasticSearchCitiesRepository;
use Config;
use Mockery;

class ElasticSearchCitiesRepositoryTest extends \Codeception\Test\Unit
{
    protected $tester;
    protected $elasticSearch;

    public function _after()
    {
        Mockery::close();
    }

    public function _before()
    {
        $this->elasticSearch = Mockery::mock(ElasticSearchCitiesRepository::class)->makepartial();
    }

    public function test_should_give_suggestions_on_elastic_search_just_query_parameters()
    {
        $source = [
            'hits' => [
                'hits' => [
                    ['_source' => 1],
                    ['_source' => 2],
                ]
            ]
        ];

        $city = Mockery::mock('overload:App\Models\City');

        $city->shouldReceive('getSearchIndex')
            ->once()
            ->shouldReceive('getSearchType')
            ->once()
            ->andReturnSelf();

        $city->shouldReceive('hydrate')
            ->once()
            ->andReturn($source);

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

        $response = $this->elasticSearch->search('Londo', null, null);

        $this->assertNotNull($response);
        $this->assertSame($suggestionResult, $response);
    }

    public function test_should_give_suggestions_on_elastic_search_with_query_latitude_parameters()
    {
        $source = [
            'hits' => [
                'hits' => [
                    ['_source' => 1],
                    ['_source' => 2],
                ]
            ]
        ];

        $city = Mockery::mock('overload:App\Models\City');

        $city->shouldReceive('getSearchIndex')
            ->once()
            ->shouldReceive('getSearchType')
            ->once()
            ->andReturnSelf();

        $city->shouldReceive('hydrate')
            ->once()
            ->andReturn($source);

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
            ->with('Londo', 42.98339, null)
            ->andReturn($suggestionResult);

        $response = $this->elasticSearch->search('Londo', 42.98339, null);

        $this->assertNotNull($response);
        $this->assertNotNull($response['suggestions']);
        $this->assertNotNull($response['suggestions']['latitude']);
        $this->assertNotNull($response['suggestions']['longitude']);
        $this->assertNotEmpty($response['suggestions']['score']);
        $this->assertSame($suggestionResult['suggestions'], $response['suggestions']);
        $this->assertSame($suggestionResult['suggestions']['latitude'], $response['suggestions']['latitude']);
        $this->assertSame($suggestionResult['suggestions']['longitude'], $response['suggestions']['longitude']);
        $this->assertSame($suggestionResult['suggestions']['score'], $response['suggestions']['score']);
    }

    public function test_should_give_suggestions_on_elastic_search_with_query_latitude_longitude_parameters()
    {
        $source = [
            'hits' => [
                'hits' => [
                    ['_source' => 1],
                    ['_source' => 2],
                ]
            ]
        ];

        $city = Mockery::mock('overload:App\Models\City');

        $city->shouldReceive('getSearchIndex')
            ->once()
            ->shouldReceive('getSearchType')
            ->once()
            ->andReturnSelf();

        $city->shouldReceive('hydrate')
            ->once()
            ->andReturn($source);

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
            ->with('Londo', 38.93345, -76.54941)
            ->andReturn($suggestionResult);

        $response = $this->elasticSearch->search('Londo', 38.93345, -76.54941);

        $this->assertNotNull($response);
        $this->assertNotNull($response['suggestions']);
        $this->assertNotNull($response['suggestions']['latitude']);
        $this->assertNotNull($response['suggestions']['longitude']);
        $this->assertNotEmpty($response['suggestions']['score']);
        $this->assertSame($suggestionResult['suggestions'], $response['suggestions']);
        $this->assertSame($suggestionResult['suggestions']['latitude'], $response['suggestions']['latitude']);
        $this->assertSame($suggestionResult['suggestions']['longitude'], $response['suggestions']['longitude']);
        $this->assertSame($suggestionResult['suggestions']['score'], $response['suggestions']['score']);
    }

    public function test_should_give_empty_suggestions_on_elastic_search_with_invalid_query_latitude_longitude_parameters()
    {
        $source = [
            'hits' => [
                'hits' => [
                    ['_source' => 1],
                    ['_source' => 2],
                ]
            ]
        ];

        $city = Mockery::mock('overload:App\Models\City');

        $city->shouldReceive('getSearchIndex')
            ->once()
            ->shouldReceive('getSearchType')
            ->once()
            ->andReturnSelf();

        $city->shouldReceive('hydrate')
            ->once()
            ->andReturn($source);

        $suggestionResult = [
            [
                'suggestions' => [
                ]
            ]
        ];

        Mockery::mock('alias:ElasticSearchCitiesRepository')
            ->shouldReceive('search')
            ->once()
            ->with('asdonasdastywqeaddo', 38.93345, -76.54941)
            ->andReturn($suggestionResult);

        $response = $this->elasticSearch->search('asdonasdastywqeaddo', 38.93345, -76.54941);

        $this->assertEmpty($response);
        $this->assertEquals($response, $suggestionResult);
        $this->assertEmpty($response['suggestions']);
    }

    public function test_should_give_empty_suggestions_on_elastic_search_with_empty_query()
    {
        $source = [
            'hits' => [
                'hits' => [
                    ['_source' => 1],
                    ['_source' => 2],
                ]
            ]
        ];

        $city = Mockery::mock('overload:App\Models\City');

        $city->shouldReceive('getSearchIndex')
            ->once()
            ->shouldReceive('getSearchType')
            ->once()
            ->andReturnSelf();

        $city->shouldReceive('hydrate')
            ->once()
            ->andReturn($source);

        $suggestionResult = [
            [
                'suggestions' => [
                ]
            ]
        ];

        Mockery::mock('alias:ElasticSearchCitiesRepository')
            ->shouldReceive('search')
            ->once()
            ->with(null,38.93345, -76.54941)
            ->andReturn('asdonasdastywqeaddo');

        $response = $this->elasticSearch->search(null, 38.93345, -76.54941);

        $this->assertEmpty($response);
        $this->assertEquals($response, $suggestionResult);
        $this->assertEmpty($response['suggestions']);
    }
}