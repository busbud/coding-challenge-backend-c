<?php namespace App\Http\Controllers;

use App\Http\Requests\SuggestionRequest;
use App\Repositories\CityRepository;
use App\Repositories\ElasticSearchCitiesRepository;
use Config;
use Illuminate\Http\Request;

class AutoCompleteController extends ApiController
{

    public function suggestions(SuggestionRequest $request)
    {
        $search = null;
        $latitude = null;
        $longitude = null;

        if ($request->has('q')) {
            $search = $request->q;

            if ($request->has('latitude')) {
                $latitude = $request->latitude;
            }

            if ($request->has('longitude')) {
                $longitude = $request->longitude;
            }
        }

        if (Config::get('services.search.enabled')) {
            $cities = ElasticSearchCitiesRepository::search($search, $latitude, $longitude);
        } else {
            $cities = CityRepository::search($search, $latitude, $longitude);
        }

        return response()->json($cities, 200);
    }

}
