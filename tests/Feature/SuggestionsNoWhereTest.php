<?php

namespace Tests\Feature;

use Illuminate\Testing\TestResponse;
use Tests\TestCase;

final class SuggestionsNoWhereTest extends TestCase
{
    private ?TestResponse $response = null;

    protected function setUp(): void
    {
        parent::setUp();
        $this->response = $this->get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere');
    }


    /**
     * with a non-existent city
     *
     * @return void
     */
    public function testWithANonExistentCityStatus()
    {
        $this->response->assertStatus(404);
    }

    /**
     * returns an empty array of suggestions
     *
     * @return void
     */
    public function testReturnsAnEmptyArrayOfSuggestions()
    {
        $this->response->assertExactJson(["suggestions" => []]);
    }

}
