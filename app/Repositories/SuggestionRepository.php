<?php

namespace App\Repositories;

use App\Imports\GeonameImport;
use Geokit\Math;
use Illuminate\Support\Str;

class SuggestionRepository
{
    public $suggestions;

    public function __construct()
    {
        $this->suggestions = [];
    }

    /**
     * @param $query
     * @param $lat
     * @param $long
     * @return array
     */
    public function search($query, $lat, $long)
    {
        $geoname_array = (new GeonameImport)->toArray(resource_path() . '/data/cities_canada-usa.tsv');

        foreach ($geoname_array as $row) {

            foreach ($row as $key => $value) {
                if ($key == 0) {
                    continue;
                }

                $cells = explode("\t", $value[0]);

                $city_population = (int)$cells[14];
                if ($city_population < 5000) {
                    continue;
                }

                $city_name = $cells[1];
                if (Str::contains(Str::lower($city_name), Str::lower($query)) === false) {
                    continue;
                }

                $city_lat = (double)$cells[4];
                $city_long = (double)$cells[5];
                $state = $cells[10];
                $country = $cells[8] == 'US' ? 'USA' : 'Canada';
                $distance = $this->calculateDistance($lat, $long, $city_lat, $city_long);

                $geo = $this->createNewGeo($city_name, $state, $country, $city_lat, $city_long, $query, $distance->km());
                if ($geo->score < 0.1) {
                    false;
                }

                $this->reorderSuggestionArray($query);

                $this->suggestions[] = $geo;
            }
        }

        return $this->suggestions;
    }

    private function createNewGeo($city_name, $state, $country, $city_lat, $city_long, $query, $distance)
    {
        $geo = new \stdClass();
        $geo->name = $city_name . ', ' . $state . ', ' . $country;
        $geo->latitude = $city_lat;
        $geo->longitude = $city_long;
        $geo->score = $this->calculateTotalScore($city_name, $query, $distance);

        return $geo;
    }

    private function calculateDistance($from_lat, $from_long, $to_lat, $to_long)
    {
        $math = new Math();
        $from = ['latitude' => $from_lat, 'longitude' => $from_long];
        $to = ['latitude' => $to_lat, 'longitude' => $to_long];
        return $math->distanceVincenty($from, $to);
    }

    private function reorderSuggestionArray($query)
    {
        usort($this->suggestions,
            function ($a, $b) use ($query) {
                if ($a->score == $b->score) {
                    if (strripos($a->name, $query) == strripos($b->name, $query)) {
                        return 0;
                    }
                    return (strripos($a->name, $query) < strripos($b->name, $query)) ? -1 : 1;
                }
                return ($a->score < $b->score) ? 1 : -1;
            }
        );
    }

    private function calculateTotalScore($city_name, $query, $distance_km)
    {
        $similarity_score = $this->calculateSimilarityScore($city_name, $query);
        $distance_score = $this->calculateDistanceScore($distance_km);

        return (float)number_format(($similarity_score + $distance_score) / 2, 1);
    }

    private function calculateSimilarityScore($first, $second)
    {
        return (1 - (float)number_format(levenshtein(Str::lower($first), Str::lower($second)) / 10, 1));
    }

    private function calculateDistanceScore($distance_km)
    {
        return (1 - (float)number_format($distance_km / 1000, 2));
    }
}
