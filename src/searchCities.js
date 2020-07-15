const esClient = require('./esClientConfig');

exports.search = async (cityPrefix, latitude, longitude) => {
  // max cities distance from the geopoint
  // the geopoint is defined by latitude and longitude
  // mandatory parameter
  const distance = '100000km';
  let searchQuery;
  if (latitude && longitude) {
    searchQuery = {
      query: {
        bool: {
          must: [
            {
              prefix: {
                'ASCIIName': {
                  'value': cityPrefix,
                },
              },
            },
            {
              geo_distance: {
                'distance': distance,
                'location': {
                  'lat': latitude,
                  'lon': longitude,
                },
              },
            },
          ],
        },
      },
    };
  } else {
    searchQuery = {
      query: {
        prefix: {
          'ASCIIName': {
            'value': cityPrefix,
          },
        },
      },
    };
  }

  const {body} = await esClient.search({
    index: 'cities',
    body: searchQuery,
  });
  return body;
};
