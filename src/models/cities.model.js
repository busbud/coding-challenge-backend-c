import fs from 'fs';
import { tsvJSON } from './../utils/functions'

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

citiesModel.getSuggestions = (queryParams) => {
    return queryParams;
};

citiesModel.getAllCities = () => {
    return jsonData;
};

export default citiesModel;