<?php

namespace Tests\Feature;

use Illuminate\Testing\AssertableJsonString;
use Illuminate\Testing\TestResponse;
use Tests\TestCase;
use Throwable;

final class GeonameSuggestionTest extends TestCase
{
    private AssertableJsonString $json_data;
    private ?TestResponse $response = null;

    protected function setUp(): void
    {
        parent::setUp(); //
        $this->response = $this->get('/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163');
        try {
            $this->json_data = $this->response->decodeResponseJson();
        } catch (Throwable $e) {
            $this->json_data = null;
        }

    }

    /**
     * returns 200
     */
    public function testResponse()
    {
        $this->response->assertStatus(200);
    }

    /**
     * returns suggestions data as array
     * @throws Throwable
     */
    public function testSuggestionsArray()
    {
        $this->assertNotNull($this->json_data);
        $this->assertInstanceOf(AssertableJsonString::class, $this->json_data);
    }

    /**
     * contains score, lat and long data
     */
    public function testDataStructure()
    {
        $this->assertNotNull($this->json_data);
        $data = json_decode($this->json_data->json);

        foreach ($data->suggestions as $geoname) {
            $this->assertTrue(isset($geoname->latitude) && isset($geoname->longitude));
            $this->assertTrue(isset($geoname->score));
        }
    }


}
