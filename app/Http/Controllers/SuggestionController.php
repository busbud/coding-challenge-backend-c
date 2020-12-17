<?php

namespace App\Http\Controllers;

use App\Http\Requests\SearchRequest;
use App\Repositories\CityRepository;
use App\Repositories\CityRepositoryInterface;

class SuggestionController extends Controller
{
    /**
     * @OA\Info(
     *     description="This is a challenge API for Busbud. Users can search destinations via location name, latitude and longitude",
     *     version="1.0.0",
     *     title="Busbud Suggestion API",
     *     termsOfService="https://www.busbud.com/en/about/tos",
     *     @OA\Contact(
     *         email="atayurtmert@gmail.com"
     *     ),
     *     @OA\License(
     *         name="Apache 2.0",
     *         url="http://www.apache.org/licenses/LICENSE-2.0.html"
     *     )
     * ),
     * @OA\Server(
     *         description="API Host",
     *         url="https://busbud-city-app.herokuapp.com/"
     *     ),
     */



    /**
     * @OA\Get(
     *     path="/suggestions",
     *     summary="Get Suggestions",
     *     tags={"suggestions"},
     *     description="Get suggestions for given search param.",
     *     operationId="getSuggestionsByParams",
     *     @OA\Parameter(
     *         name="q",
     *         in="query",
     *         description="Location Name",
     *         required=true,
     *         @OA\Schema(
     *           type="string"
     *         ),
     *         style="form"
     *     ),
     *     @OA\Parameter(
     *         name="latitude",
     *         in="query",
     *         description="Location Latitude",
     *         @OA\Schema(
     *           type="number",
     *           format="float"
     *         ),
     *         style="form"
     *     ),
     *     @OA\Parameter(
     *         name="longitude",
     *         in="query",
     *         description="Location Longitude",
     *         @OA\Schema(
     *           type="number",
     *           format="float"
     *         ),
     *         style="form"
     *     ),
     *     @OA\Response(
     *         response=200,
     *         description="successful operation",
     *         @OA\Schema(
     *             type="array",
     *             @OA\Items(ref="#/components/schemas/City")
     *         ),
     *     ),
     *     @OA\Response(
     *         response="400",
     *         description="Invalid name value",
     *     )
     * )
     */
    public function index(
        CityRepositoryInterface $cityRepository,
        SearchRequest $request)
    {

        $data = $cityRepository->search($request);

        return response()->json($data, 200);
    }
}
