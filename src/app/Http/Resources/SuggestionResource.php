<?php

namespace App\Http\Resources;

use Illuminate\Http\Resources\Json\JsonResource;

class SuggestionResource extends JsonResource
{
    /**
     * Transform the resource into an array.
     *
     * @param  \Illuminate\Http\Request  $request
     * @return array
     */
    public function toArray($request)
    {
        $scoutMetadata = $this->scoutMetadata();
        $distance = isset($scoutMetadata["_rankingInfo"]) ? $scoutMetadata["_rankingInfo"]["geoDistance"]: "N/A";
        return [
            "name" => $this->name.", ".$this->admin1_code.", ".$this->country_code,
            "latitude" => $this->latitude,
            "longitude" => $this->longitude,
            "distance" => $distance,
            "score" => $this->convertToScore($distance)
        ];
    }

    private function convertToScore($distance)
    {
        //check if geo ranking is added
        if ($distance === "N/A") {
            return $distance;
        }

        if ($distance === 0)
            return 1;

        if ($distance <= 1000 )
            return 0.9;
        if ($distance <= 2000 )
            return 0.8;
        if ($distance <= 3000 )
            return 0.7;
        if ($distance <= 4000 )
            return 0.6;
        if ($distance <= 5000 )
            return 0.5;
        if ($distance <= 6000 )
            return 0.4;
        if ($distance <= 7000 )
            return 0.3;
        if ($distance <= 8000 )
            return 0.2;
        if ($distance <= 9000 )
            return 0.1;
        return 0;
    }
}
