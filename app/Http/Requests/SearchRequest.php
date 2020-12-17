<?php

namespace App\Http\Requests;

use App\Http\Requests\Request;


/**
 * Class SearchRequest
 * @package App\Http\Requests\
 */
class SearchRequest extends Request
{
    /**
     * @return array
     */
    public function rules(): array
    {
        return [
            'q' => 'required|max:50|min:1',
            'latitude' => ['regex:/^[-]?(([0-8]?[0-9])\.(\d+))|(90(\.0+)?)$/'],
            'longitude' => ['regex:/^[-]?((((1[0-7][0-9])|([0-9]?[0-9]))\.(\d+))|180(\.0+)?)$/']
        ];
    }
}
