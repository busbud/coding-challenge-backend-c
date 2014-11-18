var mongoose = require('mongoose');
var yaml = require('js-yaml');
var fs = require('fs');
var _ = require('lodash');

var city_schema = mongoose.Schema({
  name: String,
  ascii: String,
  lat: Number,
  country: String,
  long: Number,
  admin1: String
});

city_schema.statics.cityBeLike = function(args, callback) {
  this
    .find(args.conditions)
    .sort(args.sort)
    .select(['-_id', City.NAME_FIELD, City.LATITUDE_FIELD, City.LONGITUDE_FIELD, City.COUNTRY_FIELD, City.ADMIN1_FIELD].join(' '))
    .limit(args.limit)
    .exec(callback);
};

var City = mongoose.model('City', city_schema);

City.NAME_FIELD = 'name';
City.LATITUDE_FIELD = 'lat';
City.LONGITUDE_FIELD = 'long';
City.ASCII_FIELD = 'ascii';
City.COUNTRY_FIELD = 'country';
City.ADMIN1_FIELD = 'admin1';

// Look-up tables
var admin1;
var country;
try {
  // For Canada's provinces and territories
  admin1 = yaml.safeLoad(fs.readFileSync('./data/admin1.yml', 'UTF-8'));
  // For countries
  country = yaml.safeLoad(fs.readFileSync('./data/country.yml', 'UTF-8'));
} catch (err) {
  console.warn('Could not load YAML file. Abort.');
  throw err;
}
// merge with City
_.merge(City, admin1);
_.merge(City, country);

module.exports = City;
