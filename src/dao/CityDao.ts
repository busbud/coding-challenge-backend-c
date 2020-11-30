import * as fs from 'fs';
import * as parse from 'csv-parse/lib/sync';
import * as path from 'path';

const DATA_PATH = '../../data/cities_canada-usa.tsv';
const JSON_PATH = '../../data/CityData.json';
/**
 * This dao's sole job is to either fetch content from the provided TSV file, or
 * if this file has already been read, to retrieved cached content from a json file
 */
export class CityDao {
    public read(): { [key: string]: string }[] {
        let eligibleCities: { [key: string]: string }[] = [];
        try {
            const jsonData = fs.readFileSync((path.join(__dirname, JSON_PATH))).toString();
            eligibleCities = JSON.parse(jsonData);
        } catch (e) {
            eligibleCities = this.create();
        }
        return eligibleCities;
    }
    public create():{ [key: string]: string }[]{
        const data = this.parseTSVToCityData();
        //Cache data in JSON file
        fs.writeFileSync(path.join(__dirname, JSON_PATH), JSON.stringify(data, null, 4));
        return data;
    }
    private parseTSVToCityData(): { [key: string]: string }[] {

        const dataPath = (path.join(__dirname, DATA_PATH));
        const records = parse(fs.readFileSync(dataPath), {
            delimiter: '\t', skipLinesWithError: true, skipEmptyLines: true, relax: true, columns: true, cast: true, quote: '"', ltrim: true, rtrim: true
        });
        return records;
    }
}