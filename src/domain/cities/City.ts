export default class City {
  private id: string;
  private name: string;
  private alternateName;
  private latitude: number;
  private longitude: number;
  private population: number;

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
  }

  public getPopulation() {
    return this.population;
  }
}
