<?php

namespace App\Http\Controllers;

use App\Http\Requests\SuggestionRequest;
use App\Http\Resources\SuggestionCollection;
use App\Repositories\Contracts\GeonameRepositoryContract;
use Illuminate\Http\JsonResponse;

/**
 * @OA\Info(title="GeoName", version="0.1")
 */
class SuggestionController extends Controller
{
    private GeonameRepositoryContract $repository;

    public function __construct(GeonameRepositoryContract $repository)
    {
        $this->repository = $repository;
    }

    /**
     * @OA\Get(
     *     path="/suggestions",
     *     @OA\Parameter(
     *          name="q",
     *          description="Name of city to be searched",
     *          required=true,
     *          in="query",
     *          @OA\Schema(
     *              type="string"
     *          )
     *      ),
     *     @OA\Parameter(
     *          name="latitude",
     *          description="latitude to search around",
     *          required=false,
     *          in="query",
     *          @OA\Schema(
     *              type="number"
     *          )
     *      ),
     *     @OA\Parameter(
     *          name="longitude",
     *          description="longitude to search around",
     *          required=false,
     *          in="query",
     *          @OA\Schema(
     *              type="number"
     *          )
     *      ),
     *     @OA\Parameter(
     *          name="radius",
     *          description="The radius of the bounding circle (in meters)",
     *          required=false,
     *          in="query",
     *          @OA\Schema(
     *              type="integer"
     *          )
     *      ),
     *     @OA\Response(response="200", description="Display a listing of suggestions."),
     *     @OA\Response(response="404", description="No result is found.")
     * )
     * @param SuggestionRequest $request
     * @return JsonResponse
     */
    public function index(SuggestionRequest $request): JsonResponse
    {
        $suggestedList =  $this->repository->searchGeo($request->all());

        //Empty set means resource not found
        if (count($suggestedList) == 0) {
            return response()->json(['suggestions' => []], 404);
        }
        return (new SuggestionCollection($suggestedList))
            ->response()
            ->setStatusCode(200);
    }
}
