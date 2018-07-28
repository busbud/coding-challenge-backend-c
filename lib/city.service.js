const removeDiacritics = require('diacritics').remove;
const assert = require('assert');
const Promise = require('bluebird');

class CityService {
  constructor(mongoClient, scoreService, provinceService) {
    this.db = mongoClient.db();
    this.scoreService = scoreService;
    this.provinceService = provinceService;
  }

  /*
   * Find cities in db using q then score each result and sort by score descending
   * 
   * Only 20 results are returned.
   */
  async findCities({ q, latitude, longitude }) {
    assert(q, 'q is mandatory');

    if (latitude) {
      assert(Number(latitude) >= -90 && Number(latitude) <= 90, 'Invalid latitude');
    }

    if (longitude) {
      assert(Number(longitude) >= -180 && Number(longitude) <= 180, 'Invalid longitude');
    }

    const maxResults = 20;
    const query = {
      ascii: { $regex: `^${removeDiacritics(q)}`, $options: 'si' }
    }; // the dataset contains only cities that have more than 5000 people, no need to add it in query

    let result;
    if (longitude && latitude) {
      // if user gave coords, pass them to db to make sure that even if q is very short, the city looked for is in the result
      const aggregate = [
        {
          $geoNear: {
            near: {
              type: 'Point',
              coordinates: [Number(longitude), Number(latitude)]
            },
            distanceField: 'distance',
            query,
            spherical: true
          }
        },
        { $limit: maxResults }
      ];

      result = await this.db
        .collection('cities')
        .aggregate(aggregate)
        .toArray();
    } else {
      result = await this.db
        .collection('cities')
        .find(query)
        .limit(maxResults)
        .toArray();
    }

    const formattedResult = await Promise.map(result, async city => {
      const province = await this.provinceService.getProvinceByCode(`${city.country}.${('0' + city.admin1).slice(-2)}`);

      const provinceName = province !== null ? province.name : '?';

      return {
        name: `${city.name}, ${provinceName}, ${city.country}`,
        latitude: city.lat,
        longitude: city.long,
        score: this.scoreService.score(city, { q, latitude, longitude })
      };
    });

    // sort by score, descending

    formattedResult.sort((a, b) => {
      if (a.score < b.score) return 1;
      else return -1;
    });

    return formattedResult;
  }
}

module.exports = CityService;
