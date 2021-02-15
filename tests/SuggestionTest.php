<?php

use Laravel\Lumen\Testing\DatabaseMigrations;
use Laravel\Lumen\Testing\DatabaseTransactions;

class SuggestionTest extends TestCase {

    /**
     * This test returns valid response for search.
     *
     * @return void
     */
    public function test_ValidCity() {
        $this->get('/suggestions?q=lond',['X-Busbud-Token' => 'PARTNER_BaASYYHxTxuOINEOMWq5GA']);
        $this->seeStatusCode(200);
        $this->seeJsonStructure([
                '*'=>[
                    'name',
                    'latitude',
                    'longitude',
                    'score',
                ]
            ]);

    }

    /**
     * This test returns empty search result.
     *
     * @return void
     */
    public function test_NonExistCity() {
        $this->get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere', ['X-Busbud-Token' => 'PARTNER_BaASYYHxTxuOINEOMWq5GA']);
        $this->seeStatusCode(200);
        $this->seeJsonStructure([]);

    }


}
