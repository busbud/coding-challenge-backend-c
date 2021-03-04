<?php

namespace Src\Controller;

use Src\Business\SuggestionManager;
use Src\DTO\HttpResponse;

class SuggestionController
{

    private $db;
    private $requestMethod;
    private $reponse;

    public function __construct($db, $requestMethod)
    {
        $this->db = $db;
        $this->requestMethod = $requestMethod;
        $this->reponse = new HttpResponse();
    }

    public function processRequest($data)
    {
        switch ($this->requestMethod) {
            case 'GET':
                $this->getSuggestion($data);
                break;
            default:
                $this->notFoundResponse();
                break;
        }
    }

    private function getSuggestion($data)
    {
        if (!$this->validateRequest($data)) {
            $this->reponse->unprocessableEntityResponse();
        }

        $q = $data["q"];
        $latitude = isset($data["latitude"]) ? $data["latitude"] : null;
        $longitude = isset($data["longitude"]) ? $data["longitude"] : null;

        $cityManager = new SuggestionManager($this->db);

        $cityList = $cityManager->getSuggestion($q, $latitude, $longitude);

        if (sizeof($cityList) == 0)
             $this->reponse->notFoundResponse(['suggestions' => $cityList]);

        $this->reponse->successResponse(['suggestions' => $cityList]);
    }

    private function validateRequest($input)
    {
        if (!isset($input['q']) || strlen($input['q']) == 0) {
            return false;
        }

        if (isset($input['latitude'])) {
            $latitude = floatval($input['latitude']);
            if ($latitude < -90 || $latitude > 90)
                return false;
        }

        if (isset($input['longitude']) && !is_float($input['longitude'])) {
            $latitude = floatval($input['latitude']);
            if ($latitude < -180 || $latitude > 180)
                return false;
        }

        return true;
    }
}