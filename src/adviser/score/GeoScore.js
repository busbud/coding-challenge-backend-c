
class GeoScore {
  constructor() {
    this.thresholds = [{
      distance: 500,
      score: 1,
    }, {
      distance: 1000,
      score: 0.7
    }, {
      distance: 3000,
      score: 0.5
    }, {
      distance: 6000,
      score: 0.3
    }, {
      distance: 10000,
      score: 0.2
    }, {
      distance: 20000,
      score: 0.1
    }]
  }

  getScore({ geoUserEntry, geoCity }) {
    const latDiff = (geoUserEntry.latitude - geoCity.latitude) * 110.574; // in km
    const longDiff = 111.320 * Math.cos(geoUserEntry.latitude) * geoUserEntry.longitude - geoCity.longitude; // in km
    const distance = this._hypothenuse(latDiff, longDiff);
    return this._getScore(distance);
  }

  _getScore(distance) {
    for (let i = 0; i < this.thresholds.length; i++) {
      if (this.thresholds[i].distance > distance) {
        return this.thresholds[i].score;
      }
    }
    return 0;
  }

  _hypothenuse(x, y) {
    return Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2));
  }
}

module.exports = {
  GeoScore,
};