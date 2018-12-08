import Distance from "geo-distance";

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

  public _changeScoreBy(name: string, latitude?: number, longitude?: number) {
    //latitule and longitude could be empty
    if (this.name.length === name.length) {
      this.score = 1;
    } else {
      const namePenalty = this.computeNamePenaltyWith(name);

      let distancePenalty = 0;

      if (latitude && longitude) {
        distancePenalty = this.computeDistancePenaltyWith(latitude, longitude);
      }

      const penalty = namePenalty + distancePenalty;
      const newScore = this.toFixed2(1 - penalty);

      this.score = newScore < 0 ? 0 : newScore;
    }
  }

  //a penalty of 0.05 for each letter missing
  private computeNamePenaltyWith(autocompleteValue: string): number {
    if (autocompleteValue.length > this.name.length) {
      return 0;
    }

    const result = ((this.name.length - autocompleteValue.length) / 2) * 0.1;

    return this.toFixed2(result);
  }

  //a penalty of 0.05 for each 150km
  private computeDistancePenaltyWith(
    latitude: number,
    longitude: number
  ): number {
    const distance = Number(
      Distance.between(
        { lat: latitude, lon: longitude },
        { lat: this.latitude, lon: this.longitude }
      ).human_readable().distance
    );

    const result = Math.floor(distance / 150) * 0.05;

    return this.toFixed2(result);
  }

  private toFixed2(value: number) {
    return Number(value.toFixed(2));
  }
}
