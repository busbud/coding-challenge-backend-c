const { Router } = require('express');
const { suggestionsMachinery } = require('../apimachinery');

const router = Router();

router.get('/', async (req, res) => {
  try {
    if (!req.query.q) {
      throw new Error('Missing search parameter');
    }

    const { q: searchParam, latitude, longitude } = req.query;

    if ((latitude || longitude) && (!latitude || !longitude)) {
      throw new Error(
        'Both latitude and longitude parameters are required to calculate location score'
      );
    }

    const suggestions = await suggestionsMachinery.generateSuggestions(
      searchParam,
      latitude,
      longitude
    );

    return res.json(suggestions);
  } catch (e) {
    res.status(500).json({ error: e.message });
  }
});

module.exports = router;
