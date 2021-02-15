<?php

namespace App\Http\Controllers;

use Illuminate\Support\Facades\Cache;
use Illuminate\Support\Facades\DB;
use Illuminate\Support\Facades\Request;

class SuggestionController extends Controller {
    /**
     * Create a new controller instance.
     *
     * @return void
     */
    public function __construct() {

    }



    /**
     * Main function for suggestion generate, default lat and lon based on canada
     *
     * @return mixed
     */
    public function listSuggestions() {
        $head = Request::header('X-Busbud-Token');
        if($head!='PARTNER_BaASYYHxTxuOINEOMWq5GA'){
            return response([], \Illuminate\Http\Response::HTTP_BAD_REQUEST);
        }

        $default_lat = 42.76738 ;
        $default_lon = -81.23150;


        $req = Request::capture();

        $q = $req->get('q');
        $q =trim(preg_replace("/[^A-Za-z0-9 ]/", '', $q));

        if(strlen($q)==0){
            return [];
        }


        $cac_key = 'suggestion_01_'.$q.'_'.$default_lat.'_'.$default_lon;

        return Cache::remember($cac_key, (60 * 60 * 12), function () use ($q, $default_lon, $default_lat) {

            $results = DB::table('cities_canada_usa')
                         ->select(["name",'country','state', 'lat as latitude', 'lon as longitude'])
                         ->where('population', '>=', 50000)
                         ->where('name', 'LIKE', "%{$q}%")
                         ->limit(20)
                         ->get();



            $distances = [0];

            foreach ($results as $k => $v) {
                $distances[]=$results[$k]->km = self::distance($default_lat, $default_lon, $v->latitude, $v->longitude);
            }

            $max = max($distances);
            $min = min($distances);


            $result = [];

            foreach ($results as $k => $v) {

                $result[] = [
                    'name'      => "{$v->name}, {$v->state}, {$v->country}",
                    'latitude'  => $v->latitude,
                    'longitude' => $v->longitude,
                    'score'     => (float)number_format( 1 -(($v->km * 0.9) / $max), 1),
                ];


            }

            $result = collect($result)->sortBy('score')->toArray();


            return $result;

        });




    }

    /**
     * Private helper function for Distance calculating
     * @param $lat1
     * @param $lon1
     * @param $lat2
     * @param $lon2
     * @return float
     */
    private function distance($lat1, $lon1, $lat2, $lon2) {
        $theta = $lon1 - $lon2;
        $dist  = sin(deg2rad($lat1)) * sin(deg2rad($lat2)) + cos(deg2rad($lat1)) * cos(deg2rad($lat2)) * cos(deg2rad($theta));
        $dist  = acos($dist);
        $dist  = rad2deg($dist);
        $miles = $dist * 60 * 1.1515;

        return ($miles * 1.609344);

    }

}
