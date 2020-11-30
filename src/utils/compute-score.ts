import { CityData } from '../models/CityData';
import { CityService } from '../service/CityService';
import { Score } from '../models/Score';
import { compareTwoStrings } from 'string-similarity';

const MAX_LATITUDE = 90;
const MAX_LONGITUDE = 180;

export function getCityScores(searchTerm: string, latitude: string, longitude: string,service=new CityService()): Score[] {
    let lat = null;
    let long = null;
    if (Number(latitude)) {
        lat = Number(latitude);
    }
    if (Number(longitude)) {
        long = Number(longitude);
    }if(!searchTerm){
        return [];
    }
    const searchTermLowerCase = searchTerm.toLowerCase();
    //Filter by search term on name or alternate name
    const matchingCities = service.read().filter(
        (city: CityData) => city.name.toLowerCase().startsWith(searchTermLowerCase) ||
            city.alt_name.some((name) => name.toLowerCase().startsWith(searchTermLowerCase)));

    return computeAllScores(searchTermLowerCase, matchingCities, lat, long);
}
function computeAllScores(searchTerm: string, matchingCities: CityData[], latitude: number, longitude: number, ): Score[] {
    const scores: Score[] = [];
    for (const city of matchingCities) {
    //The heuristic for scoring is how similar the inputted lat,long and name values are to the desired city
        const { latitudeScore, longitudeScore } = computeDistanceScore(latitude, longitude, city.lat, city.long);
        const nameScore = compareTwoStrings(searchTerm, city.name.toLowerCase());
        const finalScore = (longitudeScore + latitudeScore + nameScore) / 3;
        scores.push(new Score(formUniqueName(city), city.lat, city.long, finalScore));
    }
    return scores.sort((a, b) => b.score - a.score);
}
function formUniqueName(city: CityData): string {
    return `${city.name}, ${city.admin1}, ${city.country}`;
}
function computeDistanceScore(queriedLatitude: number, queriedLongitude: number, initialLatitude: number, initialLongitude: number): { longitudeScore: number, latitudeScore: number } {
    /**
     * If we dont know the longitude, we assume it is the furthest possible from the city's longitude, bringing our confidence down
     * We also need to constraint the values of the lat/long to be within possible read-world values
     */

    const maxLongDiff = MAX_LONGITUDE * 2;
    const longitudeDiff = queriedLongitude ? Math.abs(initialLongitude - clamp(queriedLongitude,-MAX_LONGITUDE,MAX_LONGITUDE)) : maxLongDiff;
    const longitudeScore = 1 - (longitudeDiff / maxLongDiff);

    const maxLatDiff = MAX_LATITUDE * 2;
    const latitudeDiff = queriedLatitude ? Math.abs(initialLatitude - clamp(queriedLatitude,-MAX_LATITUDE,MAX_LATITUDE)) : maxLatDiff;
    const latitudeScore = 1 - (latitudeDiff / maxLatDiff);

    return { latitudeScore, longitudeScore };
}

function clamp(value:number, min:number, max:number) {
    return value > max ? max : value < min ? min : value;
}
