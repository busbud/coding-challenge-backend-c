const search = require('./searchCities');
const mapperSuggestions = require('./mapCitiesToSuggestions');

exports.findCities = async function(req, res) {
  const prefixCity = req.query.q;
  if (!prefixCity) {
    return res.status(400);
  } else {
    const latitude = req.query.latitude;
    const longitude = req.query.longitude;

    const prefixCityLc = prefixCity.toLowerCase();
    const searchResult = await search.search(prefixCityLc, latitude, longitude);
    const body = await mapperSuggestions.mapToSuggestions(searchResult);

    if (body.suggestions.length ===0) {
      return res.status(404).json(body);
    }
    return res.status(200).json(body);
  }
};
