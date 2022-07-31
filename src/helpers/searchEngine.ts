import stringSimilarity from "string-similarity";
import { getDistance } from 'geolib';

export const getCitiesSuggestions = (searchParams: SearchParams, citiesSearchableObject: City[], citiesSuggestionsCache: CitiesSuggestionsCache): CitySuggestion[] => {
    if (searchParams.name === undefined || searchParams.name.length === 0) {
        return [];
    }

    searchParams = sanitizeSearchParams(searchParams);

    const cacheKey = searchParams.name + searchParams.latitude + searchParams.longitude;
    let citiesSuggestions = getCachedResponseIfExists(cacheKey, citiesSuggestionsCache);
    if (citiesSuggestions.length !== 0) {
        return citiesSuggestions;
    }

    citiesSuggestions = filterCitiesByStringSimilarity(citiesSearchableObject, searchParams.name);
    if (searchParams.latitude !== '' && searchParams.longitude !== '') {
        citiesSuggestions = filterCitiesByProximity(citiesSuggestions, parseFloat(searchParams.latitude), parseFloat(searchParams.longitude));
    }

    citiesSuggestions.sort((a, b) => b.score - a.score);
    addToCache(cacheKey, citiesSuggestions, citiesSuggestionsCache);
    return citiesSuggestions;
}

const sanitizeSearchParams = (searchParams: SearchParams): SearchParams => {

    let LATITUDE_PATTERN = "^(\\+|-)?(?:90(?:(?:\\.0{1,6})?)|(?:[0-9]|[1-8][0-9])(?:(?:\\.[0-9]{1,6})?))$";
    let LONGITUDE_PATTERN = "^(\\+|-)?(?:180(?:(?:\\.0{1,6})?)|(?:[0-9]|[1-9][0-9]|1[0-7][0-9])(?:(?:\\.[0-9]{1,6})?))$";

    return {
        name: searchParams.name.normalize("NFD").replace(/[\u0300-\u036f]/g, "").toLowerCase(),
        latitude: searchParams.latitude !== undefined && searchParams.latitude.match(LATITUDE_PATTERN) ? searchParams.latitude : '',
        longitude: searchParams.latitude !== undefined && searchParams.longitude.match(LONGITUDE_PATTERN) ? searchParams.longitude : ''
    };
}

const getCachedResponseIfExists = (cacheKey: string, citiesSuggestionsCache: CitiesSuggestionsCache): CitySuggestion[] => {
    if (citiesSuggestionsCache[cacheKey]) {
        return citiesSuggestionsCache[cacheKey];
    }
    return [];
}

const filterCitiesByStringSimilarity = (citiesSearchableObject: City[], searchName: string): CitySuggestion[] => {
    let filteredCities: CitySuggestion[] = [];
    const length = citiesSearchableObject.length
    for (var num = 0; num < length; num++) {
        let score = stringSimilarity.compareTwoStrings(citiesSearchableObject[num].name, searchName);
        filteredCities.push({
            name: citiesSearchableObject[num].name + ", " + citiesSearchableObject[num].state + ", " + citiesSearchableObject[num].country,
            latitude: citiesSearchableObject[num].latitude,
            longitude: citiesSearchableObject[num].longitude,
            score: score,
        });
    }
    return filteredCities.filter(city => city.score >= 0.33);
}

const filterCitiesByProximity = (filteredCities: CitySuggestion[], searchLatitude: number, searchLongitude: number): CitySuggestion[] => {
    const length = filteredCities.length;
    let citiesDistance: CitiesDistance = {};
    let maxDistance = 0;
    for (var num = 0; num < length; num++) {
        citiesDistance[num] = getDistance(
            { latitude: searchLatitude, longitude: searchLongitude },
            { latitude: filteredCities[num].latitude, longitude: filteredCities[num].longitude }
        );
        maxDistance = citiesDistance[num] > maxDistance ? citiesDistance[num] : maxDistance;
    }
    for (var num = 0; num < length; num++) {
        filteredCities[num].score *= 1 - (citiesDistance[num] / maxDistance);
    }
    return filteredCities.filter(city => city.score >= 0.33);
}

const addToCache = (cacheKey: string, citiesSuggestions: CitySuggestion[], citiesSuggestionsCache: CitiesSuggestionsCache) => {
    citiesSuggestionsCache[cacheKey] = citiesSuggestions;
}