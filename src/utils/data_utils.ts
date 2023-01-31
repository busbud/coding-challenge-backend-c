import {ICityRawData} from "../interfaces/raw_cities";


export function filterCitiesByCountries(cities: ICityRawData[], countryCodes: string[]): ICityRawData[] {
    const countryCodeSet = new Set(countryCodes)
    return cities.filter(c => countryCodeSet.has(c.country))
}