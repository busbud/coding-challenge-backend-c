<?php

class NonExistedCityTest extends TestCase
{
    public function testEmptyJson()
    {
        $this->json('GET', '/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
            ->seeJson()
            ->seeJsonEquals([
                "suggestions" => []
            ]);
    }

    public function testStatusCode404()
    {
        $this->json('GET', '/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
            ->seeStatusCode(404);
    }
}
