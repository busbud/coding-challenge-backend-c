const url = require('url');
const _ = require('lodash');
const { normalize } = require('../utility/normalizer');
const Cities = require('../models/Cities');

class SuggestionsController {
  constructor(esClient) {
    this.esClient = esClient;
  }

  getCities(req, res) {
    const query = url.parse(req.url, true).query;
    const searchTerm = _.get(query, 'q', '');
    const normalizedSearchTerm = normalize(searchTerm);

    if (!normalizedSearchTerm) {
      res.writeHead(400, {'Content-Type': 'text/plain'});
      res.end('No search term provided');
      return;
    }

    const opts = {
      term: normalizedSearchTerm,
      limit: 20 // limit the number of results returned
    };

    Cities.getSuggestions(this.esClient, opts).then(suggestedCities => {
      if (suggestedCities.length) {
        return suggestedCities;
      } else { // if no result, try fuzzy search
        return Cities.getFuzzySuggestions(this.esClient, opts)
      }
    }).then(suggestions => {
      // no result found!
      if (suggestions.length) {
        res.writeHead(200, {'Content-Type': 'application/json'})
      } else {
        res.writeHead(404, {'Content-Type': 'text/plain'});
      }

      res.end(JSON.stringify({ suggestions }))
    })
      .catch(() => {
      res.writeHead(500, {'Content-Type': 'text/plain'});
      res.end('Internal Server Error');
    });
  }
}

module.exports = SuggestionsController;