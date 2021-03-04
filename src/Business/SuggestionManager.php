<?php

namespace Src\Business;

use Src\Library\Utilities;
use Src\Model\City;

class SuggestionManager
{
    const MIN_POPULATION = 5000;
    const COVERED_COUNTRIES = '"US","CA"';

    private $db = null;
    private $minScore;
    private $maxScore;

    public function __construct($db)
    {
        $this->db = $db;
        $this->minScore = 0;
        $this->maxScore = 0;
    }

    public function getSuggestion($value, $latitude, $longitude)
    {
        $result = [];
        $data = $this->findMatch($value, $latitude, $longitude);

        foreach ($data as $item){
            $newScore = Utilities::Normalize($item->getScore(),$this->minScore,$this->maxScore);
            $item->setScore($newScore);
            $result[] = $item->jsonSerialize();
        }

        usort($result, Utilities::sortByKey('score'));
        return $result;
    }

    private function findMatch($value, $latitude, $longitude)
    {
        $result = $this->findMatchByWritten($value);

        if (sizeof($result) == 0)
            $result = $this->findMatchBySound($value);

        $listCity = [];
        foreach ($result as $item) {
            $entity = new City($item["name"], $item["latitude"], $item["longitude"], $item["country code"], $item["admin1 code"], $latitude, $longitude,$value);
            $currentScore = $entity->getScore();
            $this->maxScore = $currentScore > $this->maxScore ? $currentScore : $this->maxScore;
            $this->minScore = $currentScore < $this->minScore ? $currentScore : $this->minScore;
            $listCity[] = $entity;
        }
        return $listCity;
    }

    private function findMatchByWritten($value)
    {
        $statement = "SELECT * FROM `city` 
                        where LOWER(`name`) like '" . strtolower($value) . "%'
                        and `population`>=" . self::MIN_POPULATION . "
                        and `country code` in (" . self::COVERED_COUNTRIES . ")";

        $statement = $this->db->query($statement);
        return $statement->fetchAll(\PDO::FETCH_ASSOC);
    }

    private function findMatchBySound($value)
    {
        $statement = "SELECT * FROM `city` 
                        where soundex('" . $value . "') = soundex(`name`)
                        and `population`>=" . self::MIN_POPULATION . "
                        and `country code` in (" . self::COVERED_COUNTRIES . ")";


        $statement = $this->db->query($statement);
        return $statement->fetchAll(\PDO::FETCH_ASSOC);
    }
}