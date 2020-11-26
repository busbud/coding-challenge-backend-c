<?php

namespace App\Http\Resources;

use Illuminate\Http\Resources\Json\ResourceCollection;

class SuggestionCollection extends ResourceCollection
{
    public $collects = 'App\Http\Resources\SuggestionResource';
    public static $wrap = 'suggestions';

    public function toArray($request)
    {
        return parent::toArray($request);
    }
}