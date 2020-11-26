<?php

namespace App\Repositories\Contracts;

interface GeonameRepositoryContract extends BaseRepositoryContract
{
    public function searchGeo(array $params);
}