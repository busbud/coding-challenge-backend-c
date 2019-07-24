//  Node Standard
const fs = require('fs');
const readline = require('readline');
const { Transform, Duplex } = require('stream');
// General Libraries
const log4js = require('log4js');
// Application Code
const suggestionConfig = require('../config').suggestionConfig;
const admin1Code = require('../data/admin_1_code');
// Configure Logging
const logger = log4js.getLogger();
logger.level = 'debug';

/**
 * Variable used to store imported city data
 */
const data = [];

// Transform stream to parse the population of raw city
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

// Transform stream to format city coordinate
const cityCoordinateFormatter = new Transform({
  writableObjectMode: true,
  readableObjectMode: true,
  transform(city, encoding, callback) {
    // create city coordinate's latitude and longitude
    city.coordinate = {
      latitude: parseFloat(city.lat),
      longitude: parseFloat(city.long)
    };
    this.push(city);
    callback();
  }
});

// Transform stream to for city state
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

// Transform stream by rejecting cities that do not match filters
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


// Duplex stream which simply saves the final city in the in-memory storage.
const saveCity = new Duplex({
  writableObjectMode: true,
  write(city, encoding, callback) {
    data.push(city);
    callback();
  }
});


/**
 * Filters out a city based on the suggestion configuration
 * Perform the filters here so that it only needs to peform this filter on the server load
 * @param   {string}    path              The path containing the data to be imported
 * @param   {string}    encoding          Data file encoding type
 * @param   {string}    line_delimiter    Character that delimates a new line
 * @param   {function}  onComplete        function called once data stream finished
 * @return  {void}
 */
function importData(path, line_delimiter, encoding, onComplete) {
  logger.info(`Loading data: ${path}`);
  // create a file stream
  const file_stream = fs.createReadStream(path, { encoding: encoding });
  //  Note: we use the crlfDelay option to recognize all instances of carriage return &
  //  Line feed ('\r\n') in the data file as a single line break.
  const line_reader = readline.createInterface({
    input: file_stream,
    crlfDelay: Infinity
  });

  // count lines
  let line_index = 0;
  // column header storage
  let columns = [];

  // The 'line' event is emitted whenever the input stream receives an end-of-line input
  line_reader.on('line', (line) => {
    // split line by tab delimiter
    line = line.split('\t');
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
  line_reader.on('close', () => {
    cityPopulationFormatter.end();
  });

  //TODO: we could replace the saveCity pipe and simple pipe the city into PSQL for example. Since PSQL copy command supports streaming binary data directly into the table
  cityPopulationFormatter
    .pipe(cityCoordinateFormatter)
    .pipe(cityStateFormatter)
    .pipe(filterCityByConfig)
    .pipe(saveCity)
    .on('finish', function() {
      if (onComplete) {
        onComplete();
      }
    });
}

module.exports = {
  importData: importData,
  getData: () => data
};
