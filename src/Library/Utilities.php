<?php

namespace Src\Library;

class Utilities
{
    static function sortByKey($key)
    {
        return function ($a, $b) use ($key) {
            return $a[$key] >= $b[$key] ? -1 : 1;
        };
    }

    /**
     * Haversine Formula
     * From http://www.codecodex.com/wiki/Calculate_Distance_Between_Two_Points_on_a_Globe#Haversine_Formula
     * @param $userLatitude
     * @param $userLongitude
     * @param $cityLatitude
     * @param $cityLongitude
     * @return float|null
     */
    static function getDistance($userLatitude, $userLongitude, $cityLatitude, $cityLongitude)
    {
        if (!isset($userLatitude) || !isset($userLongitude))
            return null;

        $earth_radius = 6371;

        $dLat = deg2rad($cityLatitude - $userLatitude);
        $dLon = deg2rad($cityLongitude - $userLongitude);

        $a = sin($dLat / 2) * sin($dLat / 2) + cos(deg2rad($userLatitude)) * cos(deg2rad($userLongitude)) * sin($dLon / 2) * sin($dLon / 2);
        $c = 2 * asin(sqrt($a));

        return round($earth_radius * $c, 2);
    }

    static function getPath()
    {
        $uri = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
        $uri = explode('/', $uri);
        return $uri[sizeof($uri)-1];
    }

    static function getParams()
    {
        $query = parse_url($_SERVER['REQUEST_URI'], PHP_URL_QUERY);
        parse_str($query, $query);
        return $query;
    }

    static function setApiHeader()
    {
        header("Access-Control-Allow-Origin: *");
        header("Content-Type: application/json; charset=UTF-8");
        header("Access-Control-Allow-Methods: GET");
        header("Access-Control-Max-Age: 3600");
        header("Access-Control-Allow-Headers: Content-Type, Access-Control-Allow-Headers, Authorization, X-Requested-With");
    }

    static function Normalize($value, $min, $max)
    {
        return round(($value - $min) / ($max - $min),2);
    }
}