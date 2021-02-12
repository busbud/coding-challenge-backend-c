<?php

namespace App\Repositories;

use Illuminate\Database\Eloquent\Collection;
use DB;

class CityRepository implements ICityRepository
{
    public function search($query, $latitude, $longitude):array
    {
        $cities = ['suggestions' => []];

        if ($query) {

            $query = DB::table('cities')
                ->select("name", "latitude", "longitude", "similarity(name, $query) AS score")
                ->where('name', 'LIKE', "%$query%")
                ->where('population', '>', 5000);

            if (isset($latitude)) {
                $query->where('latitude', '=', $latitude);
            }

            if (isset($longitude)) {
                $query->where('longitude', '=', $longitude);
            }

            $query = $query->orderBy('score', 'desc')->get();

            array_push($cities['suggestions'], $query);
        }

        return $cities;
    }
}