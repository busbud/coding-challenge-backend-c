import provinces from '../services/loaders/ProvinceLoader';
import { City } from '../types/City';

interface ParserFromTo {
    field: string;
    parser(value: string, prev?: City);
}

const columnDictionaryParser: Map<number, ParserFromTo> = new Map<number, ParserFromTo>();

columnDictionaryParser.set(1, { field: 'name', parser: (v: string) => v });
columnDictionaryParser.set(4, { field: 'latitude', parser: (v: string) => Number.parseFloat(v) });
columnDictionaryParser.set(5, { field: 'longitude', parser: (v: string) => Number.parseFloat(v) });
columnDictionaryParser.set(8, { field: 'country', parser: (v: string) => v });
columnDictionaryParser.set(14, { field: 'population', parser: (v: string) => Number.parseInt(v, 10) });
columnDictionaryParser.set(10, { field: 'province', parser: (value: string, city: City) => provinces.get(`${city.country}.${value}`) });

export default columnDictionaryParser;