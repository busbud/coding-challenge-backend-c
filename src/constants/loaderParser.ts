import provinces from '../services/loaders/ProvinceLoader';
import { City } from '../types/City';

interface ParserFromTo {
    fieldDestination: string;
    parser(value: string, prev?: City);
}

const columnDictionaryParser: Map<number, ParserFromTo> = new Map<number, ParserFromTo>();

columnDictionaryParser.set(1, { fieldDestination: 'name', parser: (v: string) => v });
columnDictionaryParser.set(4, { fieldDestination: 'latitude', parser: (v: string) => Number.parseFloat(v) });
columnDictionaryParser.set(5, { fieldDestination: 'longitude', parser: (v: string) => Number.parseFloat(v) });
columnDictionaryParser.set(8, { fieldDestination: 'country', parser: (v: string) => v });
columnDictionaryParser.set(14, { fieldDestination: 'population', parser: (v: string) => Number.parseInt(v, 10) });
columnDictionaryParser.set(10, { fieldDestination: 'province', parser: (value: string, city: City) => provinces.get(`${city.country}.${value}`) });

export default columnDictionaryParser;