const geolib = require('geolib');
const diceCoefficient = require('../../utils/dice-coeficient');

const DISTANCE_WEIGHT = 0.3;
const NAME_SCORE_WEIGHT = 0.7;
const METERS_IN_KM = 1000;
const canadaFips = {
  '2':	"British Columbia",
  '3':	"Manitoba",
  '4':	"New Brunswick",
  '13':	"Northwest Territories",
  '7':	"Nova Scotia",
  '14':	"Nunavut",
  '8':	"Ontario",
  '9':	"Prince",
  '10':	"Quebec",
  '11':	"Saskatchewan",
  '12':	"Yukon"
};
const countries = {
  ca: 'Canada',
  us: 'United States'
};

/**
 * controllers/suggestions Module
 * @param {elasticsearch.Client} client ES client
 * @param {Module} db ./db/index.js module
 * @param {Object} config config dictionary
 */
module.exports = function (client, db, config) {
  const self = {};

  /**
   * returns an elasticsearch valid suggest query which is build by using
   * the city param
   * @param {String} city city name
   * @returns {Object} query
   */
  self.buildQuery = function (city) {
    return {
      suggest: {
        asciiSuggester: {
          prefix: city,
          completion: {
            field: 'ascii'
          }
        }
      }
    };
  };

  /**
   * Takes a list of cities and the coordinates from the client who generated
   * the request and returns an object containing all the distances normalized
   * from 0 to 1
   * @param {Array} list cities queried from db
   * @param {Object} originCoords {latitude: <float>, longitude: <float>}
   * @returns {Object} contains all distances where the key is the cityId and
   *  the value is the distance score.
   *  example:
   *  {cityId1: distanceScore1<float>, cityId2: distanceScore2<float>}
   */
  self.getDistanceScores = function (list, originCoords) {
    const distances = {};
    const scores = {}
    let max = -Infinity;
    let min = Infinity;

    // fill the distances dict with the distances in KM
    for (let item of list) {
      const distanceMts = geolib.getDistance(
        originCoords,
        {latitude: item.location.lat, longitude: item.location.lon}
      );
      const distanceKms = Math.round(distanceMts / METERS_IN_KM);

      // we need the min and max values in order to normalize the data
      max = Math.max(max, distanceKms);
      min = Math.min(min, distanceKms);
      distances[item.id] = distanceKms;
    }

    const delta = max - min;

    // fill the scores dict with the normalized values
    for (let [key, val] of Object.entries(distances)) {
      scores[key] = Math.abs(((val - min) / delta) - 1);
    }

    return scores;
  };

  /**
   * returns a list of suggestions based on a given inputCity name and
   * computates a relevance score by using the params and difference between
   * the name of the city and the inputCity name passed by the client.
   * @param {String} inputCity i.e: 'london' OR 'londo' or any string
   * @param {Object} coords [optional] A dictionary containing the
   *  geocoordinates of the client who initiated the request
   * @returns {Object} contains:
   * {
   *  name: {String} cityname, state, country
   *  latitude: {Float}
   *  longitude: {Float}
   *  score: {Float} normalize relevance score
   * }
   */
  self.get = async function (inputCity, coords) {
    // get results from db
    const response = await db.search(
      client, config.data.index, self.buildQuery(inputCity));

    // extract cities
    const list = response.body.suggest.asciiSuggester[0].options
      .map(val => val._source);

    // get all the scores mapped in a dict to each id
    const distanceScores = coords ? self.getDistanceScores(list, coords) : null;

    // if there aren't any distanceScores then the distance score must not be
    // considered to calculate the final score
    const nameWeight = distanceScores ? NAME_SCORE_WEIGHT : 1;

    // build suggestion list, generate score and sort it by score
    return suggestions = list
      .map((item) => {
        let nameScore = diceCoefficient(
          inputCity.toLowerCase(), item.ascii.toLowerCase()) * nameWeight;
        let distanceScore = distanceScores ?
          distanceScores[item.id] * DISTANCE_WEIGHT : 0;

        return {
          name: `${item.name}, ${canadaFips[item.admin1] || item.admin1}, ${countries[item.country.toLowerCase()]}`,
          latitude: item.location.lat,
          longitude: item.location.lon,
          score: Number((nameScore + distanceScore).toFixed(1))
        }
      })
      .sort((a, b) => a.score < b.score);
  };

  return self;
};
