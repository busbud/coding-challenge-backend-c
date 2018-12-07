export default class City {
  private id: string;
  private name: string;
  private countryCode: string;
  private featureCode: string;
  private latitude: number;
  private longitude: number;
  private population: number;
  private score: number;

  constructor(
    id: string,
    name: string,
    countryCode: string,
    featureCode: string,
    latitude: number,
    longitude: number,
    population: number
  ) {
    this.id = id;
    this.name = name;
    this.countryCode = countryCode;
    this.featureCode = featureCode;
    this.latitude = latitude;
    this.longitude = longitude;
    this.population = population;
    this.score = 0;
  }

  public getPopulation() {
    return this.population;
  }

  public getName() {
    return this.name;
  }

  public getLatitude() {
    return this.latitude;
  }

  public getLongitude() {
    return this.longitude;
  }

  public getCountryCode() {
    return this.countryCode;
  }

  public getScore() {
    return this.score;
  }

  public getFeatureCode() {
    return this.featureCode;
  }
}
