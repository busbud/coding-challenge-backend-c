<?php

namespace App\Repositories;

use App\Repositories\Contracts\GeonameRepositoryContract;
use Illuminate\Database\Eloquent\Collection;
use Illuminate\Support\Facades\Cache;
use Illuminate\Support\Str;

final class GeonameRepository extends BaseRepository implements GeonameRepositoryContract
{
    private const CACHE_TIME = 300;

    public function searchGeo(array $params)
    {
        $cacheKey = "v5_".http_build_query($params);

        return Cache::remember($cacheKey, self::CACHE_TIME, function () use ($params) {

            /** @var Collection $collection */
            $collection =  collect([]);

            if (isset($params["latitude"]) && isset($params["longitude"])) {
                $collection = $this->model
                    ->search($params["q"])
                    ->with([
                        'aroundLatLng' => $params['latitude'].','.$params['longitude'],
                        'aroundRadius' => $params['radius'],
                        'aroundPrecision' => 100,
                        'getRankingInfo' =>  true,
                    ])->get();
            }
            else {
                $collection = $this->model
                    ->search($params["q"])
                    ->get();
            }

            return $collection->filter(function ($item) use ($params) {
                return  Str::startsWith(Str::lower($item->asciiname), Str::lower($params["q"]));
            });
        });
    }
}