const suggestionServices = require('../services/suggestion-services');

module.exports = function(app, dbPool) {
  app.get('/suggestions', async (req, res) => {
    let q = req?.query?.q;
    q = q.replaceAll("'", "''");
    const latitude = req?.query?.latitude;
    const longitude = req?.query?.longitude;

    const searchQuery = suggestionServices.createDBQuery(q, longitude, latitude);
    const result = await dbPool.query(searchQuery);
    const cities = result.rows;
    if(latitude && longitude) {
      suggestionServices.improveSuggestionsScore(cities);
    }

    if(cities.length > 0) {
      res.status(200).json({suggestions: cities});
    } else {
      res.status(404).json({suggestions: []});
    }
  });
}
  