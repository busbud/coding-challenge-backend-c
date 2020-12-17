<?php

namespace App\Repositories;
use App\Http\Requests\SearchRequest;

/**
 * Interface CityRepositoryInterface
 * @package App\Repositories
 */

interface CityRepositoryInterface
{
    /**
     * @param eachRow $array
     * @return void
     */

     public function parseTsvData($eachRow): void;

     /**
      * @return void
      */
    public function saveData(): void;

    /**
     * @param SearchRequest $request
     * @return array
     */
    public function search(SearchRequest $request): array;

}
