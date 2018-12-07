import City from "./City";

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
}
