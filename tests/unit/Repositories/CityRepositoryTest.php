<?php

namespace Test\unit\Repositories;

use App\Repositories\CityRepository;
use Config;
use Mockery;

class CityRepositoryTest extends \Codeception\Test\Unit
{
    protected $tester;
    protected $cityRepository;

    public function _after()
    {
        Mockery::close();
    }

    public function _before()
    {
        $this->cityRepository = Mockery::mock(CityRepository::class)->makepartial();
    }

    public function test_should_give_suggestions_just_query_parameters()
    {
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

        Mockery::mock('alias:DB')
            ->shouldReceive('table')
            ->once()
            ->with('cities')
            ->shouldReceive('select')
            ->once()
            ->with("name", "latitude", "longitude", "similarity(name, 'Londo') AS score")
            ->shouldReceive('where')
            ->once()
            ->with('name', 'LIKE', "%Londo%")
            ->shouldReceive('where')
            ->once()
            ->with('population', '>', 5000)
            ->shouldReceive('orderBy')
            ->once()
            ->with('score', 'desc')
            ->andReturnSelf();


        Mockery::mock('alias:CityRepository')
            ->shouldReceive('search')
            ->once()
            ->with('Londo', null, null)
            ->andReturn($suggestionResult);

        $response = $this->cityRepository->search('Londo', null, null);

        $this->assertNotNull($response);
        $this->assertSame($suggestionResult, $response);
    }

    public function test_should_give_suggestions_with_query_latitude_parameters()
    {
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

        Mockery::mock('alias:DB')
            ->shouldReceive('table')
            ->once()
            ->with('cities')
            ->shouldReceive('select')
            ->once()
            ->with("name", "latitude", "longitude", "similarity(name, 'Londo') AS score")
            ->shouldReceive('where')
            ->once()
            ->with('name', 'LIKE', "%Londo%")
            ->shouldReceive('where')
            ->once()
            ->with('population', '>', 5000)
            ->shouldReceive('orderBy')
            ->once()
            ->with('score', 'desc')
            ->andReturnSelf();


        Mockery::mock('alias:CityRepository')
            ->shouldReceive('search')
            ->once()
            ->with('Londo', 42.98339, null)
            ->andReturn($suggestionResult);

        $response = $this->cityRepository->search('Londo', 42.98339, null);

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

    public function test_should_give_suggestions_with_query_latitude_longitude_parameters()
    {
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

        Mockery::mock('alias:DB')
            ->shouldReceive('table')
            ->once()
            ->with('cities')
            ->shouldReceive('select')
            ->once()
            ->with("name", "latitude", "longitude", "similarity(name, 'Londo') AS score")
            ->shouldReceive('where')
            ->once()
            ->with('name', 'LIKE', "%Londo%")
            ->shouldReceive('where')
            ->once()
            ->with('population', '>', 5000)
            ->shouldReceive('orderBy')
            ->once()
            ->with('score', 'desc')
            ->andReturnSelf();

        Mockery::mock('alias:CityRepository')
            ->shouldReceive('search')
            ->once()
            ->with('Londo', 38.93345, -76.54941)
            ->andReturn($suggestionResult);

        $response = $this->cityRepository->search('Londo', 38.93345, -76.54941);

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

    public function test_should_give_empty_suggestions_with_invalid_query_latitude_longitude_parameters()
    {
        $suggestionResult = [
            [
                'suggestions' => [
                ]
            ]
        ];

        Mockery::mock('alias:DB')
            ->shouldReceive('table')
            ->once()
            ->with('cities')
            ->shouldReceive('select')
            ->once()
            ->with("name", "latitude", "longitude", "similarity(name, 'Londo') AS score")
            ->shouldReceive('where')
            ->once()
            ->with('name', 'LIKE', "%asdasdsadsadasdsa%")
            ->shouldReceive('where')
            ->once()
            ->with('population', '>', 5000)
            ->shouldReceive('orderBy')
            ->once()
            ->with('score', 'desc')
            ->andReturnSelf();

        Mockery::mock('alias:CityRepository')
            ->shouldReceive('search')
            ->once()
            ->with('Londo', null, null)
            ->andReturn($suggestionResult);

        $response = $this->cityRepository->search('asdasdsadsadasdsa', null, null);

        $this->assertEmpty($response);
        $this->assertEmpty($response['suggestions']);
    }

    public function test_should_give_empty_suggestions_with_empty_query()
    {
        $suggestionResult = [
            [
                'suggestions' => [
                ]
            ]
        ];

        Mockery::mock('alias:DB')
            ->shouldReceive('table')
            ->once()
            ->with('cities')
            ->shouldReceive('select')
            ->once()
            ->with("name", "latitude", "longitude", "similarity(name, 'Londo') AS score")
            ->shouldReceive('where')
            ->once()
            ->with('name', 'LIKE', "%asdasdsadsadasdsa%")
            ->shouldReceive('where')
            ->once()
            ->with('population', '>', 5000)
            ->shouldReceive('orderBy')
            ->once()
            ->with('score', 'desc')
            ->andReturnSelf();

        Mockery::mock('alias:CityRepository')
            ->shouldReceive('search')
            ->once()
            ->with(null, null, null)
            ->andReturn($suggestionResult);

        $response = $this->cityRepository->search(null, null, null);

        $this->assertEmpty($response);
        $this->assertEmpty($response['suggestions']);
    }
}