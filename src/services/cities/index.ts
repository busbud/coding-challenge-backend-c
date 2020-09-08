import { City } from '../../types/city';
import fs from 'fs';

export type Provinces = {
  [key: string]: string;
};

export default class Cities {
  createCitiesArray(): City[] {
    return fs
      .readFileSync('./data/cities_canada-usa.tsv', 'utf-8')
      .split('\n')
      .slice(1) // Remove header line
      .map(this._parseLine)
      .filter((city: City) => this._acceptedCountry(city.country) && this._acceptedPopulation(city.population));
  }

  createProvinceList(): Provinces {
    const provinces = {};
    fs.readFileSync('./data/admin1CodesASCII.txt', 'ascii')
      .split('\n')
      .forEach((line) => {
        const values = line.split('\t');
        if (this._acceptedCountry(values[0].split('.')[0])) provinces[values[0]] = values[1];
      });

    return provinces;
  }

  private _acceptedCountry(country: string): boolean {
    return country === 'CA' || country === 'US';
  }

  private _acceptedPopulation(population: number): boolean {
    return population > 5000;
  }

  private _parseLine(line: string): City {
    const values = line.split('\t');

    return {
      id: +values[0],
      name: values[1],
      ascii: values[2],
      alt_name: values[3],
      lat: +values[4],
      long: +values[5],
      population: +values[14],
      provId: values[10],
      country: values[8],
    };
  }
}
