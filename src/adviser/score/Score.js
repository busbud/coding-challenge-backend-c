const { GeoScore } = require('./GeoScore');
const { NameScore } = require('./NameScore');

class Score {
  constructor({ }) {
    this.geoScore = new GeoScore();
    this.nameScore = new NameScore();
  }

  getScore(userEntry, city) {
    const nameScore = this.nameScore.getScore({ userEntry: userEntry.name, cityName: city.name });
    if (userEntry.latitude && userEntry.longitude) {
      const geoScore = this.geoScore.getScore({ geoUserEntry: userEntry, geoCity: city });
      return (geoScore + nameScore) / 2;
    }
    return nameScore;
  }
}

module.exports = {
  Score,
};