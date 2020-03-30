import _ from 'lodash';
import path from 'path';
import { map, filter } from 'rxjs/operators';

import { TsvParser } from '../utils';
import City from '../models/City';

/*
* Distance from the search location at which relevance scores receive half of the boost value
* Unit: km
* */
const pivot = 100;

export default class SearchCityFromFile {
  constructor ({ dataFile }) {
    this.dataFile = dataFile;
  }

  static buildFromEnv () {
    const { DATA_FILE } = process.env;
    const defaultDataFile = path.resolve('./data/cities_canada-usa.tsv');
    const cityDataFile = DATA_FILE || defaultDataFile;
    return new SearchCityFromFile({ dataFile: cityDataFile });
  }

  async search ({ q: query, latitude, longitude }) {
    const q = query.trim();
    if (_.isEmpty(q)) return Promise.reject(new Error('q params is required'));

    const tsvParser = new TsvParser({ filePath: this.dataFile });
    const regEx = new RegExp(q, 'i');
    const observableObj = tsvParser.parse()
      .pipe(
        map(data => {
          const city = new City(data);
          return {
            city,
            score: city.match(regEx)
          };
        }),
        filter(datum => datum.score),
        map(this._updateScoreWithDistance({
          latitude,
          longitude
        }))
      );

    return new Promise((resolve, reject) => {
      const results = [];
      observableObj.subscribe({
        next: (cityWithScore) => results.push(cityWithScore),
        error: (err) => reject(err),
        complete: () => resolve(results)
      });
    })
      .then(results => _.sortBy(results, (d) => -1 * d.score));
  }

  /*
  * @returns function - calculate and update relevance score
  * */
  _updateScoreWithDistance ({ latitude, longitude }) {
    return (data) => {
      const { city, score: currentScore } = data;
      let newScore = currentScore;
      if (latitude && longitude) {
        const distance = city.distanceFrom({
          lat: latitude,
          long: longitude
        });
        newScore = currentScore * (pivot / (distance + pivot));
      }
      const { id, fullName, lat, long } = city;
      return {
        id,
        name: fullName,
        latitude: lat,
        longitude: long,
        score: newScore
      };
    };
  }
}
