var mongoose = require('mongoose');

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

City.US = 'USA';
City.CA = 'Canada';

module.exports = City;
