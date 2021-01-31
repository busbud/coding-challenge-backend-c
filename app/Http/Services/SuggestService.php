<?php

namespace App\Http\Services;

use Geokit\Math;

class SuggestService
{
    /**
     * Levenshtein calculation constants, it can be tweaked for optimum results
     */
    const LEVENSHTEIN_INSERTION_COST = 1;
    const LEVENSHTEIN_REPETITION_COST = 2;
    const LEVENSHTEIN_DELETION_COST = 1;

    /**
     * The weighted arithmetic
     */
    const WEIGHT_FOR_ARITHMETIC = 5;

    /**
     * Search dataset of cities
     * @var array
     */
    protected $cities = [];

    /**
     * Search raw results
     * @var array
     */
    protected $result = [];

    /**
     * SuggestService constructor.
     * @param string $q is a query string for search
     * @param string|null $latitude is optional but required with longitude
     * @param string|null $longitude is optional but required with latitude
     */
    public function __construct(
        public string $q,
        public ?string $latitude = null,
        public ?string $longitude = null
    )
    {
        $this->cities = DataImportService::getOrImportCities();
    }

    /**
     * Parse and get results by levenshtein or if present geo location & levenshtein scoring
     * @return array
     */
    public function getResults() : array
    {
        return $this->calculateResults()
            ->sortResultsByScoreDesc()
            ->parseAndGetResultsArray();
    }

    /**
     * string similarity with levenshtein algorithm
     * @param $cityName
     * @return float
     */
    private function levenshteinScore($cityName)
    {
        // Calculate levenshtein distance
        $levenshtein = levenshtein(
            strtolower($cityName),
            strtolower($this->q),
            self::LEVENSHTEIN_INSERTION_COST,
            self::LEVENSHTEIN_REPETITION_COST,
            self::LEVENSHTEIN_DELETION_COST
        );

        // Score similarity
        $maxStrLen = max(strlen($cityName), strlen($this->q));
        $maxStrLen = $maxStrLen < 10 ? $maxStrLen : 10;

        // Calc. and return score between 0..1 float
        return 1 - $levenshtein / $maxStrLen;

    }

    /**
     * Calculate distance between city and given location
     * @param string $cityLat
     * @param string $cityLong
     * @return float
     */
    private function geoDistanceScore(string $cityLat, string $cityLong) : float
    {
        $geoMath = new Math();
        $cityLocation = ['latitude' => $cityLat, 'longitude' => $cityLong];
        $givenLocation = ['latitude' => $this->latitude, 'longitude' => $this->longitude];
        $distance = $geoMath->distanceVincenty($cityLocation, $givenLocation);

        // Calc. and return score between 0..1 float
        $score = 1 - $distance->km() / 1000;
        return $score <= 0 ? 0 : $score;
    }

    /**
     * Weighted Arithmetic Score if lat and long present
     * @param float $levenshteinScore
     * @param float|null $geoDistanceScore
     * @return float
     */
    private function getWeightedArithmeticScore(float $levenshteinScore, ?float $geoDistanceScore = null)
    {
        // https://en.wikipedia.org/wiki/Weighted_arithmetic_mean
        $sumOfWeights = self::WEIGHT_FOR_ARITHMETIC;
        if (!empty($geoDistanceScore)) {
            $sumOfWeights += 1;
        }

        return (($levenshteinScore * self::WEIGHT_FOR_ARITHMETIC) + $geoDistanceScore) / $sumOfWeights;
    }

    /**
     * Sort By Score (Desc.)
     * @return $this
     */
    private function sortResultsByScoreDesc() : SuggestService
    {
        usort($this->result, function($a, $b) {
            if ($a["score"] == $b["score"]) {
                return 0;
            }
            return ($a["score"] > $b["score"]) ? -1 : 1;
        });

        return $this;
    }

    /**
     * Get parsed results for controller
     * @param int $limit
     * @return array
     */
    private function parseAndGetResultsArray(int $limit = 10) : array
    {
        $parsedResult = [];
        for ($i = 0; $i < $limit; $i++) {
            if (!array_key_exists($i, $this->result)) {
                break;
            }

            $city = $this->result[$i];
            $parsedResult[] = [
                "name" => $city['shownName'],
                "latitude" => $city['lat'],
                "longitude" => $city['long'],
                "score" => $this->roundUp((float)$city['score'], 1),
            ];
        }
        return $parsedResult;
    }

    /**
     * Calculate results by scores
     * @return $this
     */
    private function calculateResults(): SuggestService
    {
        foreach ($this->cities as $city) {
            $levenshteinScore = $this->levenshteinScore($city['name']);
            $geoDistanceScore = null;
            if (!empty($this->latitude) && !empty($this->longitude)) {
                $geoDistanceScore = $this->geoDistanceScore($city['lat'], $city['long']);
            }

            $score = $this->getWeightedArithmeticScore($levenshteinScore, $geoDistanceScore);
            $city['score'] = $score;

            if ($score > 0) {
                $this->result[] = $city;
            }
        }

        return $this;
    }

    /**
     * Ceil the decimal value
     * @param $value
     * @param $precision
     * @return float|int
     */
    private function roundUp($value, $precision) {
        $value = (float)$value;
        $precision = (int)$precision;
        if ($precision < 0) {
            $precision = 0;
        }
        $decPointPosition = strpos($value, '.');
        if ($decPointPosition === false) {
            return $value;
        }
        $floorValue = (float)substr($value, 0, $decPointPosition + $precision + 1);
        $followingDecimals = (int)substr($value, $decPointPosition + $precision + 1);
        if ($followingDecimals) {
            $ceilValue = $floorValue + pow(10, -$precision);
        }
        else {
            $ceilValue = $floorValue;
        }

        return (float)number_format($ceilValue, 1);
    }
}
