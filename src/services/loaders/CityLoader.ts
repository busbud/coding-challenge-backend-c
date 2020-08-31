
import fs from 'fs';
import { City } from '../../types/City';
import CitySearchEngine from '../search/CitySearchEngine';
import columnDictionaryParser from '../../constants/loaderParser';
export default class CityLoader {

    loadCitiesFromTsv(fileTsv: string): Promise<any> {
        return new Promise((resolve, reject) => {
            try {
                const data = fs.readFileSync(fileTsv, 'utf8');
                CitySearchEngine.instance.initialize(this.prepareData(data));
                resolve();
            } catch (err) {
                console.error(err);
                reject(err);
            }
        });
    }

    private prepareData(rowData: string): City[] {
        return rowData
            .split('\n')
            .map(this.convertLineToCity)
            .filter((value: City) => value !== null && this.isAcceptPopulation(value?.population) && this.isAcceptCountry(value?.country))
    }

    private convertLineToCity(line: string, index: number): City {
        if (index === 0) {
            return null;
        }
        return line.split('\t')
            .reduce((prev: any, current: string, currIndex: number) => {
                if (columnDictionaryParser.get(currIndex)) {
                    return { ...prev, [columnDictionaryParser.get(currIndex).field]: columnDictionaryParser.get(currIndex).parser(current, prev) };
                }
                return prev;
            }, {})
    }

    private isAcceptCountry(country: string): boolean {
        return country === 'CA' || country === 'US';
    }

    private isAcceptPopulation(population: number): boolean {
        return population > 5000;
    }
}

