import City from "./City";
import matchSorter from "match-sorter";

export default class Cities {
  private cities: City[];

  private constructor(cities: City[]) {
    this.cities = [...cities];
  }

  public static newWithoutCities() {
    return new this([]);
  }

  public addCity(city: City) {
    this.cities.push(city);
  }

  public getCities(): ReadonlyArray<City> {
    return Object.freeze(this.cities);
  }

  public thatAutocompleteWith(
    name: string,
    longitude?: number,
    latitude?: number
  ): ReadonlyArray<City> {
    if (!name) {
      return [];
    }

    const matchResults = matchSorter(this.cities, name, { keys: ["name"] });

    return Object.freeze(matchResults.slice(0, 5));
  }
}
