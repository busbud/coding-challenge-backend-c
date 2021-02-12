<?php

namespace Test\unit\Http\Controllers;

use ApiTester;
use Config;
use Mockery;

class AutoCompleteControllerCest
{
    use Asserts;

    public function _after()
    {
        Mockery::close();
    }

    public function test_should_give_suggestions_valid_parameters(ApiTester $I){
        $I->wantTo('Request suggestions.');

        $query = 'Londo';
        $latitude = 38.93345;
        $longitude = -76.54941;

        $I->sendGET('/suggestions',[
            'q' => $query,
            'latitude' => $latitude,
            'longitude' => $longitude
        ]);

        $I->seeResponseCodeIs(200);
        $I->seeResponseIsJson();
    }

}