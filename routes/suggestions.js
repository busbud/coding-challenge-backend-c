// Node Core
const { Readable, Writable, Transform } = require('stream');
// General Libraries
const express = require('express');
const path = require('path');
const url = require('url');
const querystring = require('querystring');
// Application Code
const { getSuggestions } = require('../domain/suggestor');
var router = express.Router();
const HTTP_OK = 200;
const HTTP_BAD_REQUEST = 400;
const HTTP_NOT_FOUND = 404;

/**
 * Implements a streaming version [GET] '/suggetions/stream'
 */
router.get('/stream', async function(req, res) {
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
      } = getParameters(req.query);
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
        suggestions: suggestions.map((city) => formatCity(city))
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

  httpRequestInStream.push(JSON.stringify(req.query));        // need to push a readeable string, Buffer, or Uint8Array
  httpRequestInStream                                         //  create a readeable stream that will take the URL as a chunk
    .pipe(transformGetParameters)                             //  take the url and extra and validate parameters
    .on('error', (err) => {                                   //  getStreamedParameters may throw an error so catch it here
      res.writeHead(HTTP_BAD_REQUEST);                        //  set HTTP header
      res.end(JSON.stringify( { error: err.message }));    //  set error payload
    })
    .pipe(transformGetSuggestions)                                   //  parameters are valid and processed, get suggestions
    .pipe(writableSuggestionToHTTP);                                         //  display suggestions
});


/**
 * Implements the basig [GET] /suggestions request
 */
router.get('/', async function(req, res) {
  // fetch and validate parameters
  const {
    q,
    coordinate,
    is_valid,
    error_msg
  } = getParameters(req.query);
  if (!is_valid) {
    // return 400 if parameters aren't valid
    return res.status(HTTP_BAD_REQUEST).json({ error: error_msg });
  }
  // retrieve suggestions
  const suggestions = await getSuggestions(q, coordinate);
  // format suggestions
  res.status((suggestions.length > 0) ? HTTP_OK : HTTP_NOT_FOUND).json({
    suggestions: suggestions.map((city) => formatCity(city))
  });
});

/**
 * Formats a city to return in response
 * @param   {Object}  city            City Object
 * @param   {string}  city.name       City's display name
 * @param   {string}  city.state      City's State ISO code
 * @param   {string}  city.country    City's Coutry ISO code
 * @param   {string}  city.latitude   City's Latitude
 * @param   {string}  city.longitude  City's Longitude
 * @return  {Object}  city            Formated city object ready to display
 */
function formatCity(city) {
  return ({
    name: [city.name, city.state, city.country].join(', '),
    latitude: city.lat,
    longitude: city.long,
    score: city.score
  });
}

/**
 * Fetches and validates query parameters
 * @param   {Object}    query                                 Query Parameter
 * @return  {Object}    formatted_query                       Formatted & Validate query parameter
 * @return  {string}    formatted_query.q                     search term
 * @return  {Object}    formatted_query.coordinate            Coordinate Object
 * @return  {numeber}   formatted_query.coordinate.latitude   Coordinate's latitude
 * @return  {numeber}   formatted_query.coordinate.longitude  Coordinate's longitude
 * @return  {boolean}   formatted_query.is_valid              True if parameters are valid
 * @return  {string}    formatted_query.error_msg             Contains error message when fail validation
 */
function getParameters(query) {
  var q = query.q;
  var latitude = parseFloat(query.latitude);
  var longitude = parseFloat(query.longitude);
  var is_valid = true;
  var error_msg = '';
  // verify query string
  if (typeof q === 'undefined' || (q.length === 0)) {
    error_msg = 'Invalid query parameter';
    is_valid = false;
  }
  // longitude must be present
  if (latitude && !longitude) {
    error_msg = 'Must provide a longitude';
    is_valid = false;
  }
  // latitude must be present
  if (!latitude && longitude) {
    error_msg = 'Must provide a latitude';
    is_valid = false;
  }
  // latitude must be valid
  if (latitude && ((latitude < -90) || (latitude > 90))) {
    error_msg = 'Invalid latitude';
    is_valid = false;
  }
  // longitude must be valid
  if (longitude && ((longitude < -180) || (longitude > 180))) {
    error_msg = 'Invalid longitude';
    is_valid = false;
  }
  return {
    q: q,
    coordinate: ((latitude && longitude) ? { latitude: latitude, longitude: longitude } : null),
    is_valid: is_valid,
    error_msg: error_msg
  };
}
module.exports = router;
