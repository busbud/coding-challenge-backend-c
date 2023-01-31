import * as fs from "fs";
import * as readline from "readline";
import {tabbedGeoDataToObject} from "../utils/tsv_utils";
import {ICityRawData} from "../interfaces/raw_cities";

export async function parseCitiesDataFile(fileLocation: string): Promise<ICityRawData[]> {
    const cities: ICityRawData[] = []
    const rl = readline.createInterface({
        input: fs.createReadStream(fileLocation),
        crlfDelay: Infinity,
    });

    return new Promise((resolve) => {
        rl.on('line', (line) => {
            const city = tabbedGeoDataToObject(line.split("\t"))
            cities.push(city);
        });

        rl.on('close', () => {
            console.log("Finished reading data file");
            resolve(cities);
        })
    });
}