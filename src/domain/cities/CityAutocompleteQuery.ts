export default class CityAutocompleteQuery {
  private name: string;
  private longitude: number;
  private latitude: number;

  constructor(name: string, longitude: number, latitude: number) {
    this.name = name;
    this.longitude = longitude;
    this.latitude = latitude;
  }

  public getName() {
    return this.name;
  }

  public getLongitude() {
    return this.longitude;
  }

  public getLatitude() {
    return this.latitude;
  }
}
