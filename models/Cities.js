const url = require('url');
const _ = require('lodash');

class Cities {
  static formatHitToCity(hit) {
    return {
      name: `${hit._source.name}, ${hit._source.country}`,
      latitude: hit._source.location.lat,
      longitude: hit._source.location.lon,
      score: hit._score
    }
  }

  static buildSearchQuery(opts) {
    let termQuery = {};
    if (!opts.fuzzy) {
      termQuery = {
        'bool': {
          'should': [
            {
              'prefix': { // gives higher score to cities beginning with the search term
                'rawName': opts.term
              }
            },
            {
              'match': {
                'name': {
                  'query': opts.term,
                  'operator': 'and'
                }
              }
            }
          ]
        }
      };
    } else {
      termQuery = {
        'match': {
          'rawName': {
            'query': opts.term,
            'operator': 'and',
            'fuzziness': 'AUTO'
          }
        }
      };
    }

    // if lat long is provided, a higher score is given to closer cities from the pin
    if (opts.pin && !_.isNaN(Number(opts.pin.lat)) && !_.isNaN(Number(opts.pin.long))) {
      return {
        'size': opts.limit,
        'query': {
          'function_score': {
            'functions': [
              {
                'gauss': {
                  'location': {
                    'origin': {
                      'lat': Number(opts.pin.lat),
                      'lon': Number(opts.pin.long)
                    },
                    'scale': '1000km'
                  }
                }
              }
            ],
            'query': termQuery
          }
        }
      };
    } else {
      return {
        'size': opts.limit,
        'query': termQuery
      };
    }
  }

  static getSuggestions(esClient, opts) {
    return Promise.resolve(esClient.search({
      index: 'cities',
      body: Cities.buildSearchQuery(opts)
    })).then(result => result.hits.hits).then(hits => {
      return _.map(hits, Cities.formatHitToCity);
    });
  }
}

module.exports = Cities;