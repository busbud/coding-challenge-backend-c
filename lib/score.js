class Score {
  constructor() {}

  /**
   * @method getScore
   * @description calculate the score of the city using the searched location
   * @param {String} city
   * @param {Number} latitude
   * @param {Number} longitude
   * @returns {Number}
   */
  getScore(city, latitude, longitude) {
    const lat = Math.abs(city.latitude - latitude);
    const long = Math.abs(city.longitude - longitude);
    let score = 10 - (lat + long) / 2;
    score = score > 0 ? Math.round(score) / 10 : 0;
    return score;
  }

  /**
   * @method getResultsWithScores
   * @description map cities via adding calculated score
   * @param {Array} cities
   * @param {Number} latitude
   * @param {Numer} longitude
   * @returns {Array}
   */
  getResultsWithScores(cities, latitude, longitude) {
    return cities
      .map((city) => {
        city.score = this.getScore(city, latitude, longitude);
        return city;
      })
      .sort((a, b) => {
        return b.score - a.score;
      });
  }
}

module.exports = new Score();
