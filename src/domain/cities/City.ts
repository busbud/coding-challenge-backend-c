export default class City {
  private id: string;
  private name: string;
  private alternateName;
  private latitude: number;
  private longitude: number;
  private population: number;
  private score: number;

  constructor(
    id: string,
    name: string,
    alternateName: string,
    latitude: number,
    longitude: number,
    population: number
  ) {
    this.id = id;
    this.name = name;
    this.alternateName = alternateName;
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

  public getAlternateName() {
    return this.alternateName;
  }

  public getScore() {
    return this.score;
  }
}
