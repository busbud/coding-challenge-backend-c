
import fs from "fs";
import provinces from "./ProvinceLoader";
import City from "../../types/City";

interface ParserFromTo {
    field: string;
    parser(value: string, prev?: City);
}

const fromTo: Map<number, ParserFromTo> = new Map<number, ParserFromTo>();

fromTo.set(1, { field: 'name', parser: (v: string) => v });
fromTo.set(4, { field: 'latitude', parser: (v: string) => Number.parseFloat(v) });
fromTo.set(5, { field: 'longitude', parser: (v: string) => Number.parseFloat(v) });
fromTo.set(8, { field: 'country', parser: (v: string) => v });
fromTo.set(14, { field: 'population', parser: (v: string) => Number.parseInt(v) });
fromTo.set(10, { field: 'province', parser: (value: string, city: City) => provinces.get(`${city.country}.${value}`) });

export default class CityLoader {

    loadCitiesFromTsv(fileTsv: string): Promise<any> {
        return new Promise((resolve, reject) => {
            try {
                const data = fs.readFileSync(fileTsv, 'utf8');
                resolve(this.prepareData(data));
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

    private convertLineToCity(line: string, index: number) {
        if (index === 0) {
            return null;
        }
        return line.split('\t')
            .reduce((prev: any, current: string, currIndex: number, columns: string[]) => {
                if (fromTo.get(currIndex)) {
                    return { ...prev, [fromTo.get(currIndex).field]: fromTo.get(currIndex).parser(current, prev) };
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

