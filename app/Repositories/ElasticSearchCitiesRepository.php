<?php

namespace App\Repositories;

use App\Models\City;
use Elasticsearch\Client;
use Illuminate\Database\Eloquent\Collection;

class ElasticSearchCitiesRepository implements ICityRepository
{
    private $search;

    public function __construct(Client $client) {
        $this->search = $client;
    }

    public function search($query,$latitude,$longitude):array
    {
        $items = $this->searchOnElasticsearch($query);

        return $this->buildCollection($items)->toArray();
    }

    private function searchOnElasticsearch(string $query): array
    {
        $instance = new City;

        $items = $this->search->search([
            'index' => $instance->getSearchIndex(),
            'type' => $instance->getSearchType(),
            'body' => [
                'query' => [
                    'multi_match' => [
                        'fields' => ['name', 'latitude', 'longitude', 'score'],
                        'query' => $query,
                    ],
                ],
            ],
        ]);

        return $items;
    }

    private function buildCollection(array $items): Collection
    {
        /**
         * The data comes in a structure like this:
         * [
         *      'hits' => [
         *          'hits' => [
         *              [ '_source' => 1 ],
         *              [ '_source' => 2 ],
         *          ]
         *      ]
         * ]
         *
         * And we only care about the _source of the documents.
         */
        $hits = array_pluck($items['hits']['hits'], '_source') ?: [];

        $sources = array_map(function ($source) {
            $source['tags'] = json_encode($source['tags']);
            return $source;
        }, $hits);

        // We have to convert the results array into Eloquent Models.
        return City::hydrate($sources);
    }
}