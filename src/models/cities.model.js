import fs from 'fs';
import { tsvJSON, getCitiesThatMatchName } from './../utils/functions'

const tsvFILENAME = './data/cities_canada-usa.tsv';
const jsonFILENAME = './data/cities_canada-usa.json';

let jsonData;
// check if tsv needs to be converted to JSON
try {
    if (!fs.existsSync(jsonFILENAME)) {
        const tsvData = fs.readFileSync(tsvFILENAME, 'utf8');
        jsonData = JSON.stringify(tsvJSON(tsvData));
        fs.writeFileSync(jsonFILENAME, jsonData);
        jsonData = JSON.parse(fs.readFileSync(jsonFILENAME));
    } else {
        jsonData = JSON.parse(fs.readFileSync(jsonFILENAME));
    }
} catch (error) {
    console.error(error);
}

const citiesModel = {};

citiesModel.getSuggestions = (urlString) => {
    const fields = urlString.split('?')[1].split('&');
    let queryParams = {}

    fields.map((item) => {
        const [fieldName, givenValue] = item.split('=');
        queryParams[fieldName] = givenValue;
    });

    const filterCitiesByName = getCitiesThatMatchName(queryParams.q, jsonData);

    const suggestions = [];

    filterCitiesByName.map(city => {
        suggestions.push({
            "name": `${city.name}, ${city.admin1}, ${city.country}`,
            "latitude": city.lat,
            "longitude": city.long,
            "score": 0
        });
    });

    return JSON.stringify({ "suggestions": suggestions });
};

citiesModel.getAllCities = () => {
    return jsonData;
};

export default citiesModel;