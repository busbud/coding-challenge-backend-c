/* eslint-disable no-underscore-dangle */
const { index, client } = require('./client');
const score = require('./score.calculate');

const buildRequest = (q, latitude, longitude) => {
  const functions = [];
  if (latitude !== null && longitude !== null) {
    functions.push({
      gauss: {
        location: {
          origin: {
            lat: latitude,
            lon: longitude,
          },
          scale: '8000km',
        },
      },
    });
  }

  return {
    query: {
      function_score: {
        query: {
          bool: {
            must: [

              {
                match_bool_prefix: {
                  fullSuggestion: {
                    query: q,
                    analyzer: 'standard',
                  },
                },
              },
            ],
          },
        },
        functions,
      },
    },
    size: 5,
  };
};

const parseResponse = (response) => {
  const suggestions = response.body.hits.hits;
  if (suggestions.length === 0) {
    return [];
  }

  const maxScore = response.body.hits.max_score;
  const minScore = score.minScore(suggestions);

  return suggestions.map((suggestion) => ({
    id: suggestion._id,
    score: score.calculate(minScore, maxScore, suggestion._score),
    ...suggestion._source,
  }));
};

const fulltextSearch = async (q, latitude, longitude) => new Promise((resolve, reject) => {
  client.search({
    index,
    body: buildRequest(q, latitude, longitude),
  }).then((response) => {
    const parsed = parseResponse(response);
    resolve(parsed);
  })
    .catch((reason) => reject(reason));
});

exports.fulltextSearch = fulltextSearch;
