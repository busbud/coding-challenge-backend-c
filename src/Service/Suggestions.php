<?php

namespace App\Service;

use App\Entity\City;
use App\Repository\CityRepository;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\OptionsResolver\OptionsResolver;

class Suggestions
{
    /** @var CityRepository */
    protected $cityRepository;

    public function __construct(CityRepository $cityRepository)
    {
        $this->cityRepository = $cityRepository;
    }

    public function getSuggestionsByRequest(Request $request): array
    {
        $parameters = $this->resolveParameters($request);

        $cities = $this->cityRepository->getCitiesByQuery($parameters['q']);

        $suggestions = [];
        foreach ($cities as $city) {
            $suggestion = [
                'name' => $city->getName() . ', ' . $city->getAdmin1Code() . ', ' . $city->getCountryCode(),
                'latitude' => $city->getLatitude(),
                'longitude' => $city->getLongitude()
            ];

            if ($parameters['latitude'] && $parameters['longitude']) {
                $suggestion['distance'] = $this->calculateDistance($city, $parameters['latitude'], $parameters['longitude']);
            }

            $suggestions[] = $suggestion;
        }

        return $this->setScore($suggestions);
    }

    protected function setScore(array $suggestions): array
    {
        if (isset(current($suggestions)['distance'])) {
            usort($suggestions, function ($a, $b) {
                return ($a['distance'] < $b['distance']) ? -1 : 1;
            });
        }

        $totalScore = 1;
        $perSuggestionScore = $totalScore / count($suggestions);
        foreach ($suggestions as $index => $suggestion) {
            $suggestions[$index]['score'] = $totalScore;
            $totalScore -= $perSuggestionScore;
            $totalScore = round($totalScore, 2);

            unset($suggestions[$index]['distance']);
        }

        return $suggestions;
    }

    protected function calculateDistance(City $city, $latitude, $longitude): float
    {
        $piMultiplier = 57.3; // roughly 180/pi
        $milToLat = 69.1; // miles to latitude degrees multiplier

        $cityLatitudeCos = cos($city->getLatitude() / $piMultiplier);

        return sqrt(pow(($milToLat * ($city->getLatitude() - $latitude)), 2) + (pow(($milToLat * ($longitude - $city->getLongitude()) * $cityLatitudeCos), 2)));
    }

    protected function resolveParameters(Request $request): array
    {
        $resolver = $this->getParametersResolver();

        return $resolver->resolve($request->query->all());
    }

    protected function getParametersResolver(): OptionsResolver
    {
        $resolver = new OptionsResolver();

        $resolver->setRequired(['q']);

        $resolver->setDefined([
            'latitude',
            'longitude'
        ]);

        $resolver->setDefaults([
            'latitude' => null,
            'longitude' => null
        ]);

        $resolver->setAllowedTypes('q', ['string']);
        $resolver->setAllowedTypes('latitude', ['string', 'null']);
        $resolver->setAllowedTypes('longitude', ['string', 'null']);

        return $resolver;
    }
}