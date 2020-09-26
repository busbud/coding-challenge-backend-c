const mongoose = require('mongoose');
const Schema = mongoose.Schema;
const stringCompare = require('string-similarity');
const calculateDistance = require('../helpers/distance');

const CitySchema = new Schema({
  name: {
    type: String,
    required: true,
  },
  country: {
    type: String,
    required: true,
  },
  region: {
    type: String,
    required: false,
  },
  population: {
    type: Number,
    required: true,
  },
  timezone: {
    type: String,
    required: true,
  },
  location: {
    type: {
      type: String,
      enum: ['Point'],
      required: true,
    },
    coordinates: {
      type: [Number],
      required: true,
    },
  },
  modified: {
    type: Date,
    required: false,
  },
});

CitySchema.index({location: '2dsphere'});

CitySchema.statics.parse = (city, query, coordinates) => {
  let score = stringCompare.compareTwoStrings(city.name.toLowerCase(), query.q ? query.q.toLowerCase() : '');
  if (coordinates) {
    const distance = calculateDistance(
        city.location.coordinates[0],
        city.location.coordinates[1],
        query.longitude,
        query.latitude,
    );
    score = score / 4;
    score += (distance < 1000) ? (999 - distance) / 1332 : 0;
  }
  return {
    name: `${city.name}, ${city.region}, ${city.country}`,
    latitude: city.location.coordinates[1],
    longitude: city.location.coordinates[0],
    score: parseFloat(score).toFixed(2),
  };
};

const City = mongoose.model('City', CitySchema);
module.exports = City;
