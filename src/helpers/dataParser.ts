import { readFileSync } from 'fs';
import * as d3Dsv from 'd3-dsv';

export const getCitiesSearchableObject = (): City[] => {
    let parseMappedArray: d3Dsv.DSVParsedArray<City>;

    const citiesFileContent = readFile('./data/cities_canada-usa.tsv');

    parseMappedArray = d3Dsv.tsvParse(citiesFileContent, function (rawRow) {
        let rr: d3Dsv.DSVRowString = rawRow;
        let pr: City;

        if (parseInt(rr['population']!) > 5000) {
            pr = {
                name: rr['name']!.normalize("NFD").replace(/[\u0300-\u036f]/g, "").toLowerCase(),
                country: getCountryFullName(rr['country']!),
                state: rr['country'] === 'US' ? rr['admin1']! : getCanadianStateByKey(rr['admin1']!),
                latitude: rr['lat']!,
                longitude: rr['long']!,
            };

            return pr;
        }

    });

    return parseMappedArray;
}

const readFile = (filePath: string): string => {
    const file = readFileSync(filePath, 'utf-8');
    return file;
}

const getCountryFullName = (countryAbv: string) => {
    switch (countryAbv) {
        case 'US':
            return 'USA';
        case 'CA':
            return 'Canada'
        default:
            return '';
    }
}

const getCanadianStateByKey = (key: string): string => {
    const canadianStates: CanadianStates = {
        "01": "AB",
        "02": "BC",
        "03": "MB",
        "04": "NB",
        "05": "NL",
        "07": "NS",
        "08": "ON",
        "09": "PE",
        "10": "QC",
        "11": "SK",
        "12": "YT",
        "13": "NT",
        "14": "NU",
    };
    return canadianStates[key];
}