import geolib from 'geolib';
import {score as fuzzScore} from 'fuzzaldrin';

const METERS_IN_KM = 1000;
const PRECISION_HUNDRED_METERS = 100;


const computeNameScore = (city: any, query: string): number => {
    const {name, ascii, alt_name: altName} = city;

    const doFuzzy = (queryTerm: string) => (againstTerm: string) => fuzzScore(againstTerm,queryTerm);
    const fuzzQueryScore = doFuzzy(query);

    const altNamesArray = altName.split(',');

    // Find back where was the match
    const [bestAltScore] = altNamesArray.map(fuzzQueryScore).sort((a: number,b: number): any=> b-a);
    const [nameScore, asciiScore] = [name,ascii].map(fuzzQueryScore);

    const bestScore = Math.max(nameScore,asciiScore,bestAltScore);

    return bestScore;
};

export const computeNameScoreAndDistance = (location: any,  city: any, query: string): any => {
    const nameScore = computeNameScore(city,query);
    if (!location) {
        return {...city, distance: 1, nameScore};
    }
    const {lat: latitude, long: longitude} = city;
    let distanceInMeters: number = geolib.getDistance(location, {latitude,longitude});
    return {...city, distance: distanceInMeters, nameScore};
};

const normalizeZeroToOne = (val: number, minVal: number, maxVal: number, newMin: number=0, newMax: number=1) => {
    return newMin + (val - minVal) * (newMax - newMin) / (maxVal - minVal);
};


const scoreFormula = (distanceScore: number,nameScore: number) => distanceScore*0.8 + nameScore*0.2;

export const scoringNameAndDistance = (city: any,minDistance: number,maxNameScore: number,suggestionsLength: number): any => {
    let {distance, nameScore} = city;
    if (distance === 0) {
        distance = 1;
    }
    if (minDistance === 0) {
        minDistance = 1;
    }
    const upperBound = scoreFormula(1,maxNameScore);
    const currentScore = scoreFormula(minDistance/distance,nameScore);
    const score = parseFloat(normalizeZeroToOne(currentScore,0,upperBound).toPrecision(6));
    return {...city,score};
};
