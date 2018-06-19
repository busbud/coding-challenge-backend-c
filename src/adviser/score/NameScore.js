const assert = require('assert');

class NameScore {
  constructor() {
    this.pointsFound = 2;
    this.pointsSamePositionOrFollowed = 3;
  }

  getScore({ userEntry, cityName }) {
    assert(userEntry, 'expected userEntry');
    assert(cityName, 'expected cityName');

    const points = this._getPoints(userEntry, cityName);
    return this._transformToPercentage(points, userEntry);
  }

  _getPoints(userEntry, cityName) {
    const userEntryArray = userEntry.split('');
    let points = 0;
    userEntryArray.forEach((letter, index) => {
      const cityNameIndex = cityName.indexOf(letter); // TODO find all indexes if repeted letter
      if (cityNameIndex === index) {
        points += this.pointsSamePositionOrFollowed;
      } else if (cityNameIndex >= 0) {
        points += this.pointsFound;
      }
    });
    if (userEntry.length < cityName.length) {
      points -= (cityName.length - userEntry.length);
    }
    return points;
  }

  _transformToPercentage(points, userEntry) {
    const max = userEntry.length * this.pointsSamePositionOrFollowed;
    return points / max;
  }
}

module.exports = {
  NameScore,
};