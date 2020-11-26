<?php

namespace Tests\Feature;

use Illuminate\Testing\AssertableJsonString;
use Illuminate\Testing\TestResponse;
use Tests\TestCase;
use Throwable;

final class SuggestionsValidCityTest extends TestCase
{
    private ?TestResponse $response = null;
    private AssertableJsonString $jsonString;

    protected function setUp(): void
    {
        parent::setUp(); //
        $this->response = $this->get('/suggestions?q=Montreal');
        try {
            $this->jsonString = $this->response->decodeResponseJson();
        } catch (Throwable $e) {
            $this->jsonString = null;
        }

    }

    /**
     * returns a 200
     */
    public function testSuccessResponse()
    {
        $this->response->assertStatus(200);
    }

    /**
     * returns an array of suggestions
     * @throws Throwable
     */
    public function testReturnsArrayOfSuggestions()
    {
        $this->assertNotNull($this->jsonString);
        $this->assertInstanceOf(AssertableJsonString::class, $this->jsonString);
    }

    /**
     * contains latitudes and longitudes
     */
    public function testValidateShapeOfDataReturned()
    {
        $this->assertNotNull($this->jsonString);
        $suggestions = json_decode($this->jsonString->json);

        foreach ($suggestions->suggestions as $item) {
            $this->assertTrue(isset($item->latitude) && isset($item->longitude));
        }
    }

    /**
     * contains scores
     */
    public function testContainsScores()
    {
        $this->assertNotNull($this->jsonString);
        $suggestions = json_decode($this->jsonString->json);

        foreach ($suggestions->suggestions as $item) {
            $this->assertTrue(isset($item->score));
        }
    }


}
