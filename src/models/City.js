import { DistanceHelper } from '../utils';

export default class City {
  constructor ({ id, name, alt_name: altName, lat, country, admin1, tz, long, population }) {
    this.id = Number(id);
    this.name = name;
    this.alt_name = altName;
    this.lat = Number(lat);
    this.country = country;
    this.admin1 = admin1;
    this.tz = tz;
    this.long = Number(long);
    this.population = population;
  }

  /*
  * @returns null|number - Match score
  * */
  match (reg) {
    const matchInfo = reg.exec(this.name);
    return matchInfo && this._getBoostScore(matchInfo);
  }

  distanceFrom ({ lat, long }) {
    const R = 6378.137;
    const p1Lat = DistanceHelper.rad(this.lat);
    const p1Long = DistanceHelper.rad(this.long);
    const p2Lat = DistanceHelper.rad(lat);
    const p2Long = DistanceHelper.rad(long);
    const diffLat = p1Lat - p2Lat;
    const diffLong = p1Long - p2Long;
    const d = 2 * R * Math.asin(Math.sqrt(Math.sin(diffLat / 2) * Math.sin(diffLat / 2) +
      Math.cos(p1Lat) * Math.cos(p2Lat) * Math.sin(diffLong / 2) * Math.sin(diffLong / 2)));
    return Math.abs(DistanceHelper.round2Decimal(d));
  }

  _getBoostScore (matchInfo) {
    const matchLength = matchInfo[0].length;
    return DistanceHelper.round2Decimal(matchLength / (this.name.length));
  }
}
