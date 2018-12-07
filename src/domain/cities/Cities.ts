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

    //Limit is to 20 before scoring
    //this is because matchSorter is doing a great job, and to get the best 5 results, scoring 20 results is enougth
    const matchResults: City[] = matchSorter(this.cities, name, {
      keys: ["name"]
    })
      .slice(0, 20)
      .map((city: City) => {
        city._changeScoreBy(name, latitude, longitude);
        return city;
      })
      .sort((c1: City, c2: City) => c2.getScore() - c1.getScore());

    return Object.freeze(matchResults.slice(0, 5));
  }
}
