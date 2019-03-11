const mongoose = require('mongoose');
const Schema = mongoose.Schema;
const CitySchema = new Schema({
  geonameid: Number,
  name: String,
  asciiname: String,
  latitude: Number,
  longitude: Number,
  country_code: String,
  admin1: String,
  admin2: String,
  display_name: String
});
const CityModel = mongoose.model('City', CitySchema);
module.exports = CityModel;