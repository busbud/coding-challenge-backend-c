<?php

namespace Src\Model;

use Src\Library\Utilities;

class City implements \JsonSerializable
{
    const SHORT_DISTANCE_MAX = 200; // Km
    const MEDIUM_DISTANCE_MAX = 1000;
    const SHORT_DISTANCE_SCORE = 30;
    const MEDIUM_DISTANCE_SCORE = 20;
    const LONG_DISTANCE_SCORE = 10;



    private string $name;
    private float $latitude;
    private float $longitude;
    private string $country;
    private $adminCode1;
    private ?float $distanceFromUserLocation;
    private float $score;

    public function __construct($name, $latitude, $longitude, $country,$adminCode1, $userLatitude, $userLongitude,$q)
    {
        $this->name = $name;
        $this->latitude = $latitude;
        $this->longitude = $longitude;
        $this->country = $country;
        $this->adminCode1 =  $adminCode1;
        $this->distanceFromUserLocation = Utilities::getDistance($userLatitude, $userLongitude, $latitude, $longitude);
        $this->score = $this->getInitScore($q);
    }

    /**
     * @return string
     */
    public function getName() : string
    {
        return $this->name;
    }

    public function setName($name)
    {
        $this->name = $name;
    }

    /**
     * @return mixed
     */
    public function getCountry() : string
    {
        return $this->country;
    }

    /**
     * @param mixed $country
     */
    public function setCountry(string $country)
    {
        $this->country = $country;
    }

    /**
     * @return mixed
     */
    public function getLatitude() : float
    {
        return $this->latitude;
    }

    /**
     * @param mixed $latitude
     */
    public function setLatitude(float $latitude)
    {
        $this->latitude = $latitude;
    }

    /**
     * @return mixed
     */
    public function getLongitude() : float
    {
        return $this->longitude;
    }

    /**
     * @param mixed $longitude
     */
    public function setLongitude(float $longitude)
    {
        $this->longitude = $longitude;
    }

    /**
     * @return mixed
     */
    public function getDistanceFromUserLocation() : float
    {
        return $this->distanceFromUserLocation == null ? 0 : $this->distanceFromUserLocation;
    }

    public function jsonSerialize(): array
    {
        return
            [
                "name" => $this->getName().", " .$this->adminCode1. ", " . $this->getCountry(),
                "latitude" => $this->getLatitude(),
                "longitude" => $this->getLongitude(),
                "score" => $this->getScore(),
                "distance" => $this->getDistanceFromUserLocation() ." Km"
            ];
    }


    private function getScoreRelatedToDistance(): int
    {
        if ($this->distanceFromUserLocation == null)
            return 0;

        if($this->distanceFromUserLocation <= self::SHORT_DISTANCE_MAX )
            return self::SHORT_DISTANCE_SCORE;

        if($this->distanceFromUserLocation<= self::MEDIUM_DISTANCE_MAX)
            return self::MEDIUM_DISTANCE_SCORE;

        return self::LONG_DISTANCE_SCORE;
    }

    private function getScoreRelatedToName($q)
    {
        similar_text(strtolower($q),strtolower($this->name),$perc);
        return $perc;
    }

    public function getInitScore($q)
    {
        $scoreByDistance = $this->getScoreRelatedToDistance() * 40 / 100;
        $scoreByName = $this->getScoreRelatedToName($q) * 60 / 100;
        return $scoreByName + $scoreByDistance;
    }

    public function setScore($score)
    {
        $this->score = $score;
    }

    /**
     * @return int
     */
    public function getScore() : float
    {
        return $this->score;
    }
}