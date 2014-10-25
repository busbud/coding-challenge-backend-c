var csv  = require('ya-csv');
var City = require('./city');

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
        self.cities.push(new City(data));
      }
    })
    .on('end', function() {
      console.log('Finished loading cities.');
      done(self.cities);
    });
};

module.exports = Reader;
