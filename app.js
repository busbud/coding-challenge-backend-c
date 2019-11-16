var http = require('http');
var port = process.env.PORT || 2345;

var url = require('url');

const config = require('./modules/global');
const fileSystem = require('./modules/fileSystem');
const googleMaps = require('./modules/googleMaps');

const mySuggestions = require('./modules/mySuggestions');

module.exports = http
  .createServer(function(req, res) {
    res.writeHead(404, { 'Content-Type': 'text/plain' });

    if (req.url.indexOf('/suggestions') === 0) {
      let parametersObj = getParametersFromQueryString(req);

      let arrayDestinations = fileSystem.getMatchingCities(parametersObj.name);
      if (arrayDestinations.length <= 0) {
        res.end(
          JSON.stringify({
            suggestions: [],
          })
        );
      }

      googleMaps
        .getDistances(parametersObj, arrayDestinations)
        .then((r) => {
          //r.map((d) => console.log(`${d.name} - ${d.distance} km`));
          const arraySuggestions = mySuggestions.getRanking(
            parametersObj.name,
            arrayDestinations,
            config.WEIGHT_NAME,
            config.WEIGHT_DISTANCE
          );

          const suggestions = arraySuggestions.map((city) => ({
            name: [city.name, city.adminCode1, city.countryCode].join(', '),
            latitude: city.latitude,
            longitude: city.longitude,
            //distance: city.distance,
            score: parseFloat(city.score).toFixed(config.NUM_DECIMALS),
          }));

          res.end(
            JSON.stringify({
              suggestions: suggestions,
            })
          );
        })
        .catch((err) => {
          const arraySuggestions = mySuggestions.getRanking(
            parametersObj.name,
            arrayDestinations,
            1,
            0
          );
          const suggestions = arraySuggestions.map((city) => ({
            name: [city.name, city.adminCode1, city.countryCode].join(', '),
            latitude: city.latitude,
            longitude: city.longitude,
            //distance: city.distance,
            score: parseFloat(city.score).toFixed(config.NUM_DECIMALS),
          }));
          res.end(
            JSON.stringify({
              suggestions: suggestions,
            })
          );
        });
    } else {
      res.end();
    }
  })
  .listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);

/**
 * From the URL, each parameter is obtained
 * latitud and longitud are parsed to float type in case of existence
 *
 * @param {request}
 * @returns {{name: String, latitude?: Number, longitude?: Number}} parameters
 */
const getParametersFromQueryString = (req) => {
  const parsedUrl = url.parse(req.url, true);
  const queryAsObject = parsedUrl.query;
  let qLatitude = parseFloat(queryAsObject.latitude);
  let qLongitude = parseFloat(queryAsObject.longitude);
  return {
    name: queryAsObject.q || '',
    latitude: qLatitude > -90 && qLatitude < 90 ? qLatitude : undefined,
    longitude: qLongitude > -180 && qLongitude < 180 ? qLongitude : undefined,
  };
};
