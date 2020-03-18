import fs from 'fs';
import { tsvJSON, getCitiesThatMatchName, calcNameMatchingScore, calcDistanceScore,
    sortByDescendingScore } from './../utils/functions'

const tsvFILENAME = './data/cities_canada-usa.tsv';
const jsonFILENAME = './data/cities_canada-usa.json';

let jsonData;

const citiesModel = {};

citiesModel.getSuggestions = (urlString) => {
    // check if tsv needs to be converted to JSON
    try {
        if (!fs.existsSync(jsonFILENAME)) {
            const tsvData = fs.readFileSync(tsvFILENAME, 'utf8');
            fs.writeFileSync(jsonFILENAME,
                JSON.stringify(
                    tsvJSON(tsvData)
                        .filter((city) => {
                            // read json data and filter for Canadian and American cities with >5000 population
                            return ['CA', 'US'].includes(city.country) && city.population > 5000;
                        })
                )
            );
        }

        jsonData = JSON.parse(fs.readFileSync(jsonFILENAME));
    } catch (error) {
        console.error(error);
    }

    const fields = urlString.split('?')[1].split('&');
    let queryParams = {}

    // Possible queryParams q/latitude/longitude. q is used for the city name
    fields.map((item) => {
        const [fieldName, givenValue] = item.split('=');
        queryParams[fieldName] = givenValue;
    });

    const filterCitiesByName = getCitiesThatMatchName(queryParams.q, jsonData);

    const suggestions = [];

    filterCitiesByName.map(city => {
        // if long and lat are given calculate distance score otherwise distance score is 1 to not skew name matching score
        const distanceScore =
            queryParams.latitude && queryParams.longitude
                ? calcDistanceScore([queryParams.latitude, queryParams.longitude], [city.lat, city.long])
                : 1

        const nameMatchingScore = calcNameMatchingScore(city.name, queryParams.q);
        const score = Math.round(((distanceScore + nameMatchingScore) / 2) * 10) / 10;

        suggestions.push({
            "name": `${city.name}, ${city.admin1}, ${city.country}`,
            "latitude": city.lat,
            "longitude": city.long,
            score
        });
    });

    return { "suggestions": suggestions.sort(sortByDescendingScore) };
};

export default citiesModel;