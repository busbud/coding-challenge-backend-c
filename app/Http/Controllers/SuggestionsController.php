<?php

namespace App\Http\Controllers;

use App\Http\Services\SuggestService;
use Illuminate\Http\Request;
use Illuminate\Http\Response;

class SuggestionsController extends Controller
{
  public function getSuggestions(Request $request)
    {
        $this->validate($request, [
            'q' => 'required',
            'latitude' => ['required_with:longitude','regex:/^[-]?(([0-8]?[0-9])\.(\d+))|(90(\.0+)?)$/'],
            'longitude' => ['required_with:latitude','regex:/^[-]?((((1[0-7][0-9])|([0-9]?[0-9]))\.(\d+))|180(\.0+)?)$/']
        ]);

        $suggestService = new SuggestService($request->get('q'), $request->get('latitude'), $request->get('longitude'));
        $suggestions = $suggestService->getResults();
        $statusCode = 200;
        if (empty($suggestions)) {
            $statusCode = 404;
        }

        return (new Response(['suggestions' => $suggestions], $statusCode))
            ->header('Content-Type', 'application/json; charset=utf-8');
    }
}
