const url = require('url');
const _ = require('lodash');
const { normalize } = require('../utility/normalizer');

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
              'prefix': {
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
    return {
      'size': opts.limit,
      'query': termQuery
    };
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