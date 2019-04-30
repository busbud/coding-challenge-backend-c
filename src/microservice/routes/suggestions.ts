import {suggestionsConfig} from './config';
import {BadRequestError} from '../../middlewares/error';
import fileStreamParser from '../../utils/fileStreamParser';

import {computeNameScoreAndDistance, scoringNameAndDistance} from '../../utils/scoring';

const {suggestFrom, minPopulation} = suggestionsConfig;

const cache: { [key: string]: any } = {
    states: {},
    cities: {},
    countries: {},
};

const isInCache = (key: string): number => Object.keys(cache[key]).length;

const getCountries = async (): Promise<any> => {
    if (isInCache('countries')) {
        return cache.countries;
    }
    const countries = await fileStreamParser({
        file: 'countries.txt',
        filter: (country, accumulator): void => {
            const {ISO} = country;
            if (suggestFrom.has(ISO)) {
                accumulator[ISO] = country;
            }
        },
        headers: 'auto',
        accumulator: {}, // Object to fill
    });
    cache.countries = countries;
    // countries are a hash at this point looking like:
    /**
        {
            CA: {ISO: "CA", ISO3: "CAN", ISO-Numeric: "124", …}
            US: {ISO: "US", ISO3: "USA", ISO-Numeric: "840", …}
        }
    */
    return countries;
};

const getStates = async (): Promise<any> => {
    if (isInCache('states')) {
        return cache.states;
    }
    const states = await fileStreamParser({
        file: 'admin1Codes.txt',
        filter: (state, accumulator): void => {
            const {code} = state;
            const codeStart = code.slice(0, 2);
            if (suggestFrom.has(codeStart)) {
                accumulator[code] = state;
            }
        },
        headers: ['code','name','nameAscii','geonameID'],
        accumulator: {}, // Object to fill
    });
    cache.states = states;
    // states are a hash at this point looking like:
    /**
        {
            CA.01: {code: "CA.01", name: "Alberta", nameAscii: "Alberta", …}
            CA.02: {code: "CA.02", name: "British Columbia", nameAscii: "British Columbia", …}
            ...
            US.AK: {code: "US.AK", name: "Alaska", nameAscii: "Alaska", …}
            US.AL: {code: "US.AL", name: "Alabama", nameAscii: "Alabama", …}
        }
    */
    return states;
};

// Super long to do on each run but at least
// you get to see usage of streams.
// Ideally tsvToJson would emit so we could act
// on a new data on the fly instead of gathering the full json
// I mean it's not everyday a new country gets added anyway.
const getCities = async (): Promise<any> => {
    if (isInCache('cities')) {
        return cache.cities;
    }
    const cities = await fileStreamParser({
        file: 'cities_canada-usa.tsv',
        filter: (city, accumulator): void => {
            const {population, country} = city;
            if ((parseInt(population,10) > minPopulation) && suggestFrom.has(country)) {
                accumulator.push(city); // Todo: batch push is more gently on the cpu
            }

            accumulator.push(city);
        },
        headers: 'auto',
        accumulator: [], // Object to fill
    });
    cache.cities = cities;
    // cities are an array of objects at this point looking like:
    /**
        [
            {id: "5881791", name: "Abbotsford", ascii: "Abbotsford", …}
            {id: "5882142", name: "Acton Vale", ascii: "Acton Vale", …}
            {id: "5882799", name: "Airdrie", ascii: "Airdrie", …}
            {id: "5882873", name: "Ajax", ascii: "Ajax", …}
        ]
    */
    return cities;
};

const suggestionsEndpoint = async (req: any, res: any): Promise<any> => {
    const {q,latitude,longitude} = req.query;

    if (!q) {
        throw new BadRequestError('Parameter q required');
    }
    const countries = await getCountries();
    const states = await getStates();
    const cities = await getCities();

    const cleanedQuery = q.toLowerCase().replace(/-/g, ' ').replace(/\s\s+/g, ' ').trim();

    const filteredSuggestions = cities.filter((city: any): boolean => {
        let {name, ascii, alt_name: altName} = city;
        const match = [name, ascii]
            .map((key): string => key.toLowerCase().replace('-',' '))
            .some((elem): boolean => elem.startsWith(cleanedQuery));
        const altNameMatch = altName.split(',').some((elem: string): boolean => elem.startsWith(cleanedQuery));
        return match || altNameMatch;
    });
    if (!filteredSuggestions.length) {
        res.status(404);
    }

    let location: any = null;
    if (latitude && longitude) {
        location = {
            latitude: parseFloat(latitude),
            longitude: parseFloat(longitude)
        };
    }

    let minDistance = Infinity;
    let maxNameScore = 0;

    const suggestionsWithDistance = filteredSuggestions
        .map((city: any): any => {
            const computedCity = computeNameScoreAndDistance(location, city, q);
            if (computedCity.distance < minDistance) {
                minDistance = computedCity.distance;
            }
            if (computedCity.nameScore > maxNameScore) {
                maxNameScore = computedCity.nameScore;
            }
            return computedCity;
        })
        .map((city: any): any => {
            const scoredCity = scoringNameAndDistance(city, minDistance, maxNameScore, filteredSuggestions.length);
            return scoredCity;
        })
        .map((city: any, index: number): any => {
            const {name, country, admin1, lat: latitude, long: longitude, score} = city;
            const normalizedName = name.normalize('NFD').replace(/[\u0300-\u036f]/g, '');
            return {
                name: `${normalizedName}, ${
                    states[`${country}.${admin1}`].name || admin1
                }, ${countries[country].ISO}`,
                latitude,
                longitude,
                score: parseFloat((score - score*(index/100)).toFixed(2)),
            };
        });


    const cleanedDuplicates = suggestionsWithDistance.reduce((accumulator: any[],elem: any): any[] => {
        const alreadyIn = accumulator.find((city: any) => city.name === elem.name && city.latitude === elem.latitude && city.longitude === elem.longitude);
        if (!alreadyIn) {
            accumulator.push(elem);
        }
        return accumulator;
    },[]);

    const suggestions = cleanedDuplicates
        .sort((cityA: any,cityB: any) => cityB.score - cityA.score)
        .slice(0,10);

    res.json({suggestions});
};

export default suggestionsEndpoint;
