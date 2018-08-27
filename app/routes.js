const { Router } = require('express');
const services = require('./services');

const routes = Router();


routes.get('/suggestions', async (req, res) => {
  const { q, latitude, longitude } = req.query;
  const query = services.sanitizeValue(q);

  // Try to load from cache first.
  let cities = await services.loadFromCache(query, latitude, longitude);
  if (cities) {
    return res.status(200).json({ suggestions: JSON.parse(cities) });
  }

  // If we're still going, there was nothing in the cache.
  try {
    // Query the cities from the database.
    cities = await services.queryCities(latitude, longitude);
  } catch (err) {
    console.error(err.message);
    return res.status(500).send('Query error');
  }

  // Rank and filter cities based on various criteria.
  const rankedCities = services.rankCities(cities, query, latitude && longitude);
  const filteredCities = services.filterCities(rankedCities);

  // For simple query searches, cache the results in Redis for quick access.
  services.saveToCache(query, filteredCities, latitude, longitude);

  return res.status(filteredCities.length ? 200 : 404).json({ suggestions: filteredCities });
});

module.exports = { routes };
