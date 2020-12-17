<?php

namespace App\Repositories;

use App\Models\City;
use App\Http\Requests\SearchRequest;
use Illuminate\Support\Facades\Redis;
use Illuminate\Support\Facades\DB;

/**
 * Class CityRepository.
 */
class CityRepository implements CityRepositoryInterface
{
    private $city;
    const MAX_LIMIT = 10;
    const MAX_DISTANCE_IN_MILES = 1000;

    private $insertDataList = [];


    public function __construct(City $city)
    {
        $this->city = $city;
    }

    public function parseTsvData($eachRow): void
    {
        $parsedArray = [];

        foreach ($eachRow as $index => $rowItem) {
            $parsedArray[$this->city->fillable[$index]] = $rowItem;
        }
        array_push($this->insertDataList, $parsedArray);
    }

    public function saveData(): void
    {
        foreach (array_chunk($this->insertDataList, 100) as $eachChunk) {
            City::insert($eachChunk);
        }
    }

    public function setDataToRedis()
    {

        Redis::hmap('cities', '');
    }

    //Buraya doclar gelcek
    public function search(SearchRequest $request): array
    {

        $suggestions = ['suggestions' => []];

        if ($request->filled('longitude') && $request->filled('latitude')) {
            $long = $request->get('longitude');
            $lat = $request->get('latitude');
            $dist = self::MAX_DISTANCE_IN_MILES;

            $longLatCities = DB::table('cities as t1')
                ->select(
                    't1.name',
                    't1.lat',
                    't1.long',
                    DB::raw("3956 * 2 *
                                    ASIN(SQRT(POWER(SIN((" . $lat . " - t1.lat)*pi()/180/2),2)
                                    +COS(" . $lat . "*pi()/180 )*COS(t1.lat*pi()/180)
                                    *POWER(SIN((" . $long . "-t1.long)*pi()/180/2),2)))
                                    as distance")
                )
                ->whereRaw(DB::raw("t1.long between (" . $long . "-$dist/cos(radians(" . $lat . "))*69)
                                and ($long+$dist/cos(radians($lat))*69)
                                and t1.lat between ($lat-($dist/69))
                                and ($lat+($dist/69))"))
                ->havingRaw('distance < ?', [$dist])
                ->orderBy('distance')
                ->take(self::MAX_LIMIT)
                ->get();

            foreach ($longLatCities as $index => $result) {
                unset($result->distance);
                $result->score = $this->getScore(self::MAX_LIMIT - $index);
                array_push($suggestions['suggestions'], $result);
            }
        } else {
            $results = City::where('name', 'like', $request->get('q') . '%')
                ->select('name', 'lat', 'long')
                ->take(self::MAX_LIMIT)
                ->get();

            foreach ($results as $index => $result) {
                $result['score'] = $this->getScore(self::MAX_LIMIT - $index);
                array_push($suggestions['suggestions'], $result);
            }
        }

        return $suggestions;
    }

    /**
     * @param int $position
     * @return float
     */
    private function getScore(int $position): float
    {
        return $position / 10;
    }
}
