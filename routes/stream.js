// Node Core
const fs = require('fs');
const readline = require('readline');
const { Transform } = require('stream');
// General Libraries
const express = require('express');
// Application Code
const { searchString, scoreCity } = require('../domain/suggestor.helper');
const { getSuggestionParameters, serializeCity } = require('./routes.helper');
const admin1Code = require('../data/admin_1_code');
const suggestionConfig = require('../config').suggestionConfig;
const { DATA_PATH, DATA_DELIMITER, HTTP_OK, HTTP_BAD_REQUEST, HTTP_NOT_FOUND } = require('../constants');
var router = express.Router();


/**
 * Implements a streaming version [GET] '/'
 *  1. Create a readable stream from tsv file
 *  2. Pipe into city population formatter
 *  3. Pipe into city coordinate formatter
 *  4. Pipe into city state formatter
 *  5. Pipe into filter by city based on config
 *  6. Pipe into search by search term
 *  7. Pipe into add score to city
 *  8. Pipe into city to json formatter
 *  9. Pipe into suggetions array
 */
router.get('/', async function(req, res) {
  // fetch suggestion parameters and processing them
  const { q, coordinate, is_valid, error_msg } = getSuggestionParameters(req.query);
  // count lines
  let line_index = 0;
  // column header storage
  let columns = [];
  // store suggestions
  let suggestions = [];

  // validating parameters
  if (!is_valid) {
    // return 400 if parameters aren't valid
    return res.status(HTTP_BAD_REQUEST).json({ error: error_msg });
  }

  // create a readble stream from TSV file
  const data_file_path = DATA_PATH;
  const file_stream = fs.createReadStream(data_file_path);

  // format city's population
  const cityPopulationFormatter = new Transform({
    writableObjectMode: true,
    readableObjectMode: true,
    transform(city, encoding, callback) {
      // update city populations
      city.population = parseInt(city.population);
      this.push(city);
      callback();
    }
  });

  // format city's coordinate
  const cityCoordinateFormatter = new Transform({
    writableObjectMode: true,
    readableObjectMode: true,
    transform(city, encoding, callback) {
      city.coordinate = {
        latitude: parseFloat(city.lat),
        longitude: parseFloat(city.long)
      };
      this.push(city);
      callback();
    }
  });

  // format city's state
  const cityStateFormatter = new Transform({
    writableObjectMode: true,
    readableObjectMode: true,
    transform(city, encoding, callback) {
      // get state key mapping
      const stateKey = [city.country, city.admin1].join('.');
      // get state information
      const state = admin1Code[stateKey];
      // update city state infomation
      city.state = (state ? state.isocode2 : city.admin1);
      // pas city downstream
      this.push(city);
      callback();
    }
  });

  // filter city by config
  const filterCityByConfig = new Transform({
    writableObjectMode: true,
    readableObjectMode: true,
    transform(city, encoding, callback) {
      var has_valid_min_population = (suggestionConfig.minPopulation ? city.population >= suggestionConfig.minPopulation : true);
      var has_valid_max_population = (suggestionConfig.maxPopulation ? city.population <= suggestionConfig.maxPopulation : true);
      var has_valid_country = (suggestionConfig.countryWhitelist ? suggestionConfig.countryWhitelist.includes(city.country) : true);
      if (has_valid_min_population && has_valid_max_population && has_valid_country) {
        this.push(city);
      }
      callback();
    }
  });

  // search city by query search term
  const searchCityBySearchTerm = new Transform({
    writableObjectMode: true,
    readableObjectMode: true,
    transform(city, encoding, callback) {
      const searchState = searchString(city.ascii, q);
      if (searchState.found) {
        this.push(city);
      }
      callback();
    }
  });

  // add score to city
  const addScoreToCity = new Transform({
    writableObjectMode: true,
    readableObjectMode: true,
    transform(city, encoding, callback) {
      city['score'] = scoreCity(city, q, coordinate);
      this.push(city);
      callback();
    }
  });

  // formats city to json
  const cityToJson = new Transform({
    writableObjectMode: true,
    readableObjectMode: true,
    transform(city, encoding, callback) {
      const serializedCity = serializeCity(city);
      this.push(serializedCity);
      callback();
    }
  });

  // push city to suggestions array
  const pushCityToSuggestions = new Transform({
    writableObjectMode: true,
    readableObjectMode: true,
    transform(city, encoding, callback) {
      suggestions.push(city);
      callback();
    }
  });

  // create a read line interface to process file stream line by line
  const rl = readline.createInterface({
    input: file_stream,
    crlfDelay: Infinity
  });

  // on new line event
  rl.on('line', (line) => {
    // split line by delimiter
    line = line.split(DATA_DELIMITER);
    if (line_index !== 0) {
      // process data line
      var data_line = {};
      // map header names to line value and create map
      columns.forEach((column, columnIndex) => {
        data_line[column] = line[columnIndex];
      });
      // write data_line map into a stream
      cityPopulationFormatter.write(data_line);
    } else {
      // process header line
      columns = line;
    }
    line_index++;
  });

  // on file close event
  rl.on('close', () => {
    cityPopulationFormatter.end();
  });

  cityPopulationFormatter
    .pipe(cityCoordinateFormatter)
    .pipe(cityStateFormatter)
    .pipe(filterCityByConfig)
    .pipe(searchCityBySearchTerm)
    .pipe(addScoreToCity)
    .pipe(cityToJson)
    .pipe(pushCityToSuggestions)
    .on('finish', function() {
      // sort suggestions
      suggestions = suggestions.sort(function(city_a, city_b) {
        // sort cities
        return (city_b.score - city_a.score);
      });
      res
        .status((suggestions.length > 0) ? HTTP_OK : HTTP_NOT_FOUND)
        .json({ suggestions: suggestions});
    });
});

module.exports = router;
