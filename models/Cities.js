const url = require('url');
const _ = require('lodash');
const { normalize } = require('../utility/normalizer');

class Cities {
  static getSuggestions(esClient, opts) {
    const query = {
      'size': opts.limit,
      'query': {
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
      }
    };

    return Promise.resolve(esClient.search({
      index: 'cities',
      body: query
    })).then(result => result.hits.hits).then(hits => {
      return _.map(hits, hit => ({
        name: hit._source.name,
        latitude: hit._source.location.lat,
        longitude: hit._source.location.lon,
        score: hit._score
      }));
    });
  }

  static getFuzzySuggestions(esClient, opts) {
    const query = {
      'size': opts.limit,
      'query': {
        'match': {
          'rawName': {
            'query': opts.term,
            'operator': 'and',
            'fuzziness': 'AUTO'
          }
        }
      }
    };

    return Promise.resolve(esClient.search({
      index: 'cities',
      body: query
    })).then(result => result.hits.hits).then(hits => {
      return _.map(hits, hit => ({
        name: hit._source.name,
        latitude: hit._source.location.lat,
        longitude: hit._source.location.lon,
        score: hit._score
      }));
    });
  }
}

module.exports = Cities;