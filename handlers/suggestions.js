const url = require('url');

const esClient = require('../services/elasticsearch');

function formatCityName(name) {
  return name;
}

function search(q, location) {
  return esClient.search({
    index: 'cities',
    body: {
      query: {
        function_score: {
          query: {
            bool: {
              should: [{ prefix: { name: q } }, { prefix: { alternative_names: q } }],
            },
          },
          functions: location
            ? [
              {
                gauss: {
                  location: {
                    origin: location,
                    offset: '5km',
                    scale: '500km',
                  },
                },
              },
            ]
            : [],
        },
      },
    },
  });
}

module.exports = async (req) => {
  try {
    const { q, latitude, longitude } = url.parse(req.url, true).query;

    const results = await search(
      q,
      latitude && longitude ? { lat: parseFloat(latitude), lon: parseFloat(longitude) } : undefined,
    );

    if (results.hits.total === 0) {
      return {
        status: 404,
        body: { suggestions: [] },
      };
    }

    return {
      status: 200,
      body: results.hits.hits.map(({ _score: score, _source: { name, location: { lat, lon } } }) => ({
        name: formatCityName(name),
        latitude: lat,
        longitude: lon,
        score,
      })),
    };
  } catch (err) {
    return {
      status: 500,
      body: null,
    };
  }
};
