const Helper = require('./Helper');

module.exports = class FuzzyCitySearch {
  constructor(haystack = [], keys = [], options = {}) {
    this.haystack = haystack;
    this.keys = keys;
    this.options = Object.assign({
      caseSensitive: false,
      sort: false,
    }, options);
  }

  search(query = '', latitude, longitude) {
    if (query === '') {
      return [];
    }

    const results = [];
    let minDistance;
    let minMatch;

    for (let i = 0; i < this.haystack.length; i++) {
      const item = this.haystack[i];

      if (this.keys.length === 0) {
        const match = FuzzyCitySearch.isMatch(item, query, this.options.caseSensitive);

        if (match) {
          item.match = match;
          results.push(item);
        }
      } else {
        for (let y = 0; y < this.keys.length; y++) {
          const propertyValues = Helper.getDescendantProperty(item, this.keys[y]);

          let found = false;

          for (let z = 0; z < propertyValues.length; z++) {
            const match = FuzzyCitySearch.isMatch(propertyValues[z], query, this.options.caseSensitive);

            if (match) {
              found = true;

              item.match = match;
              results.push(item);

              break;
            }
          }

          if (found) {
            break;
          }
        }
      }

      // capture the lowest match score to be used for final score aggregation
      if (minMatch > item.match || (!minMatch && item.match)) {
        minMatch = item.match;
      }

      if (longitude && latitude) {
        item.distance = FuzzyCitySearch.distance(latitude, longitude, item.latitude, item.longitude);

        // capture the smallest distance to be used for final score aggregation
        if (minDistance > item.distance || (!minDistance && item.distance)) {
          minDistance = item.distance;
        }
      }
    }

    // aggregate score over
    // - query match score
    // - distance
    results.map(result => {
      const match = minMatch / result.match;

      if (longitude && latitude) {
        const distance = minDistance / result.distance;
        result.score = (match + distance) / 2;
      } else {
        result.score = match
      }

      delete result.match;
      delete result.distance;
    })

    // sort by highest score
    if (this.options.sort) {
      results.sort((a, b) => b.score - a.score);
    }

    return results;
  }

  static isMatch(item, query, caseSensitive) {
    if (! caseSensitive) {
      item = item.toLocaleLowerCase();
      query = query.toLocaleLowerCase();
    }

    const letters = query.split('');
    const indexes = [];

    let index = 0;

    for (let i = 0; i < letters.length; i++) {
      const letter = letters[i];

      index = item.indexOf(letter, index);

      if (index === -1) {
        return false;
      }

      indexes.push(index);

      index++;
    }

    if (item === query) {
      return 1;
    }

    return indexes.reduce((a, b) => a + b, 2);
  }

  // copied from the link down below
  // https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula
  static distance(lat1, lon1, lat2, lon2) {
    const p = 0.017453292519943295;    // Math.PI / 180
    const c = Math.cos;
    const a = 0.5 - c((lat2 - lat1) * p)/2 +
            c(lat1 * p) * c(lat2 * p) *
            (1 - c((lon2 - lon1) * p))/2;

    return 12742 * Math.asin(Math.sqrt(a)); // 2 * R; R = 6371 km
  }

}
