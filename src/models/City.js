const mongoose = require("mongoose");

const CitySchema = new mongoose.Schema({
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
  population: Number,
});

CitySchema.index({ name: 1 }, { name: "name_index" });
CitySchema.index({ population: 1 }, { population: "population_index" });

module.exports = mongoose.model("City", CitySchema);
