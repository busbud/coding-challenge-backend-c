const {mongoose} = require('./mongoose');

const Schema = new mongoose.Schema({
  name: {
    type: String,
    required: true,
  },
  latitude: {
    type: Number,
    required: true,
  },
  longitude: {
    type: Number,
    required: true,
  },
  population: {
    type: Number,
    required: true,
  },
  country: {
    type: String,
    required: true,
  }
});

const Suggestion = mongoose.model('Suggestion', Schema, 'Suggestions');

module.exports = {Suggestion};
