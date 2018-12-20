const {Router} = require('express');

const {getRelevantCities} = require('../lib/citiesFinder');
const {validateQuery, validateCoordinates} = require('../utils/validators');


const routes = Router();

routes.get('/suggestions', async (request, response, next) => {
  try {
    const {q = '', latitude, longitude} = request.query;

    // Check if query is valid
    const {valid, errors} = validateQuery(q);
    if (!valid) {
      return response.status(400).json({errors});
    }

    // Check if coordinates are valid.
    let coordinates;
    if (latitude && longitude) {
      const {valid, errors} = validateCoordinates(latitude, longitude);
      if (!valid) {
        return response.status(400).json({errors});
      }
      coordinates = {latitude, longitude};
    }

    // Get and returns relevant cities
    const relevantCities = await getRelevantCities(q, coordinates);
    return (
      response
        .status(relevantCities.length ? 200 : 404)
        .json({
          suggestions: relevantCities,
        })
    );
  } catch (e) {
    return next(e);
  }
});

routes.get('*', (request, response) => response.send('Not Found', 404));

module.exports = routes;