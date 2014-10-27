var csv  = require('ya-csv');
var City = require('./city');

// City id generator
var city_id_counter = 1;
function getNewCityId() {
  city_id_counter++;
  return city_id_counter;
}

// City Reader class
function Reader() {
  this.cities = [];
}

Reader.prototype.load = function(path, done) {
  var self = this;
  var csv_load_options = {
    columnsFromHeader: true,
    quote: null,
    escape: null,
    separator: '\t'
  };

  csv.createCsvFileReader(path, csv_load_options)
    .on('data', function(data) {
      if (data.population > 5000) {
        var city_id = getNewCityId();
        self.cities.push( new City(data) );
      }
    })
    .on('end', function() {
      console.log('Finished loading cities.');
      done(self.cities);
    });
};

module.exports = Reader;
