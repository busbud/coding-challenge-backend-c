const url = require('url');

const CANADA_PROVINCES = {
  1: 'AB',
  2: 'BC',
  3: 'MB',
  4: 'NB',
  5: 'NL',
  7: 'NS',
  8: 'ON',
  9: 'PE',
  10: 'QC',
  11: 'SK',
  12: 'YT',
  13: 'NT',
  14: 'NU',
};

const COUNTRIES = {
  CA: 'Canada',
  US: 'USA',
};

function formatCityName({ name, adminCode, countryCode }) {
  let admin = adminCode;
  if (countryCode === 'CA') {
    admin = CANADA_PROVINCES[parseInt(adminCode, 10)];
  }

  return `${name}, ${admin}, ${COUNTRIES[countryCode]}`;
}

function search(esClient, q, location) {
  return esClient.search({
    index: 'cities',
    body: {
      query: {
        function_score: {
          query: {
            bool: {
              should: [
                { match_phrase_prefix: { name: q } },
                { match_phrase_prefix: { alternative_names: q } },
              ],
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

module.exports = ({ esClient }) => async (req) => {
  try {
    const { q, latitude, longitude } = url.parse(req.url, true).query;

    const results = await search(
      esClient,
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
      body: {
        suggestions: results.hits.hits.map(({
          _score: score,
          _source: {
            name,
            admin_code: adminCode,
            country_code: countryCode,
            location: { lat, lon },
          },
        }) => ({
          name: formatCityName({ name, adminCode, countryCode }),
          latitude: lat,
          longitude: lon,
          score,
        })),
      },
    };
  } catch (err) {
    return {
      status: 500,
      body: null,
    };
  }
};
