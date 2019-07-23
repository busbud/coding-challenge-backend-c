// Node Core
const { Readable, Writable, Transform } = require('stream');
// General Libraries
const express = require('express');
const { getData } = require('../lib/loadData');
// Application Code
const { getSuggestions } = require('../domain/suggestor');
const { searchString, scoreCity, cleanAndNormalizeString } = require('../domain/suggestor.helper');
const { getSuggestionParameters, serializeCity  } = require('./routes.helper');
var router = express.Router();
const HTTP_OK = 200;
const HTTP_BAD_REQUEST = 400;
const HTTP_NOT_FOUND = 404;

/**
 * Implements a streaming version [GET] '/steam/beta
 *
 */
router.get('/beta', async function(req, res) {

  // fetch suggestion parameters and processing them
  const { q, coordinate, is_valid, error_msg } = getSuggestionParameters(req.query);

  // validating parameters
  if (!is_valid) {
    // return 400 if parameters aren't valid
    return res.status(HTTP_BAD_REQUEST).json({ error: error_msg });
  }

  let cities = getData();
  // creates a readable string from the cities store in-memory
  const cityDataStream = new Readable({
    read(size) {
      this.push(JSON.stringify(cities[this.cityIndex]));
      this.cityIndex += 1;
      if (this.cityIndex > cities.length) {
        this.push(null);
      }
    }
  });
  cityDataStream.cityIndex = 0;

  // filters city by search term
  const cityFilter = new Transform({
    readableObjectMode: true,
    transform(cityChunk, encoding, callback) {
      const city = JSON.parse(cityChunk.toString());
      const searchState = searchString(city.ascii, q);
      if(searchState.found) {
        this.push(city);
      }
      callback();
    }
  });

  // adds score to city
  const cityScorer = new Transform({
    writableObjectMode: true,
    readableObjectMode: true,
    transform(city, encoding, callback) {
      city['score'] = scoreCity(city, q, coordinate);
      this.push(city);
      callback();
    }
  });

  // formats city to json
  const cityFormatter = new Transform({
    writableObjectMode: true,
    readableObjectMode: true,
    transform(city, encoding, callback) {
      this.push(JSON.stringify(serializeCity(city)));
      callback();
    }
  });

  cityDataStream
    .pipe(cityFilter)
    .pipe(cityScorer)
    .pipe(cityFormatter)
    .pipe(res);
});



/**
 * Implements a streaming version [GET] '/steam/alpha'
 */
router.get('/alpha', async function(req, res) {
  // get parameters transform
  const transformGetParameters = new Transform({
    readableObjectMode: true,
    transform(chunk, encoding, callback) {
      // get  buffered chunk and convert to string
      const queryUrl = chunk.toString();
      // parse url string
      const parsedUrl = JSON.parse(queryUrl);
      // retrieve parameters
      const {
        q,
        coordinate,
        is_valid,
        error_msg
      } = getSuggestionParameters(req.query);
      // validate parameters
      if (!is_valid) {
        // Call callbox once we are done processing with error
        callback(new Error(error_msg)); //
        return
      }
      this.push({q, coordinate});
      // Call callback once we are done processing without error
      callback();
    }
  });
  // get suggestions transform
  const transformGetSuggestions = new Transform({
    writableObjectMode: true,
    readableObjectMode: true,
    transform(params, encoding, callback) {
      getSuggestions(params.q, params.coordinate).then(function(suggestions){
        // Call callback once we are done processing without error
        this.push(suggestions);
        callback();
      }.bind(this)); // binding this is require to gain access to push
    }
  });
  // writes suggestions to http
  const writableSuggestionToHTTP = new Writable({
    objectMode: true,
    write(suggestions, encoding, callback) {
      res.writeHead(((suggestions.length > 0) ? HTTP_OK : HTTP_NOT_FOUND));
      res.end(JSON.stringify({
        suggestions: suggestions.map((city) => serializeCity(city))
      }));
      callback();
    }
  });
  // implements http stream
  const httpRequestInStream = new Readable({
    // implement read function to push data on demand
    read(size) {
      // there is a demand on the data... Someone wants to read it.
      this.push(null);
    }
  });

  httpRequestInStream.push(JSON.stringify(req.query));            // need to push a readeable string, Buffer, or Uint8Array
  httpRequestInStream                                             //  create a readeable stream that will take the URL as a chunk
    .pipe(transformGetParameters)                                 //  take the url and extra and validate parameters
    .on('error', (err) => {                                       //  getStreamedParameters may throw an error so catch it here
      res.writeHead(HTTP_BAD_REQUEST);                            //  set HTTP header
      res.end(JSON.stringify( { error: err.message }));    //  set error payload
    })
    .pipe(transformGetSuggestions)                                //  parameters are valid and processed, get suggestions
    .pipe(writableSuggestionToHTTP);                              //  display suggestions
});

module.exports = router;