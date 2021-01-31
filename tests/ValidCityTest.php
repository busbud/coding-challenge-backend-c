<?php

class ValidCityTest extends TestCase
{
    public function testJson()
    {
        $this->json('GET', '/suggestions?q=Montreal')
            ->seeJson()
            ->seeJsonContains([
                "name" => "Montréal, QC, Canada"
            ]);
    }

    public function testStatusCode200()
    {
        $this->json('GET', '/suggestions?q=Montreal')
            ->seeStatusCode(200);
    }
}
