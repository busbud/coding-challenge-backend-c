const models = require('../models');

module.exports = router => {
  router.get('/suggestions', getSuggestions);
};

async function getSuggestions(req, res) {
  const {
    latitude,
    longitude,
    q,
  } = req.query;

  // Some basic validations
  if(typeof q === 'undefined' || !q.length) return res.status(400).json({error: `q (query) parameter is required and must be at least one character.`});
  if((!latitude && !!longitude) || (!longitude && !!latitude)) {
    return res.status(400).json({error: `Either provide both latitude and longitude or none at all.`});
  }

  const suggestions = await models.cities.getSuggestions({q, latitude, longitude});

  res.status(suggestions.length ? 200 : 404).json({suggestions});
}
