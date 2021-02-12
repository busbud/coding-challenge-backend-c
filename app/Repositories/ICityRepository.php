<?php

namespace App\Repositories;

use Illuminate\Database\Eloquent\Collection;

interface ICityRepository
{
    public function search($query,$latitude,$longitude):array;
}