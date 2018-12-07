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

  public _changeScoreBy(name: string, latitude: number, longitude: number) {
    //latitule and longitude could be empty
    if (this.name.length === name.length) {
      this.score = 1;
    } else {
      const namePenalty = this.computeNamePenalty(name);
      this.score = 1 - namePenalty;
    }
  }

  //a penalty of 0.05 is applied for each letter missing
  private computeNamePenalty(autocompleteValue: string): number {
    if (autocompleteValue.length > this.name.length) {
      return 0;
    }

    return ((this.name.length - autocompleteValue.length) / 2) * 0.1;
  }
}
