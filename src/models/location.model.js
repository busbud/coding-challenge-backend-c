const mongoose = require('mongoose');

const Schema = mongoose.Schema;

const locationSchema = new Schema(
  {
    id: {
      type: Number,
      required: true,
      unique: true,
    },
    name: {
      type: String,
      required: true,
      minlength: 1,
      maxlength: 200,
    },
    ascii: {
      type: String,
      minlength: 1,
      maxlength: 200,
    },
    alt_name: {
      type: String,
      maxlength: 5000,
    },
    lat: {
      type: Number,
    },
    long: {
      type: Number,
    },
    feat_class: {
      type: String,
      maxlength: 1,
    },
    feat_code: {
      type: String,
      maxlength: 10,
    },
    country: {
      type: String,
      maxlength: 2,
    },
    cc2: {
      type: String,
      maxlength: 60,
    },
    admin1: {
      type: String,
      maxlength: 20,
    },
    admin2: {
      type: String,
      maxlength: 20,
    },
    admin3: {
      type: String,
      maxlength: 20,
    },
    admin4: {
      type: String,
      maxlength: 20,
    },
    population: {
      type: Number,
      required: true,
    },
    elevation: {
      type: Number,
    },
    dem: {
      type: Number,
    },
    tz: {
      type: String,
      maxlength: 40,
    },
    modified_at: {
      type: Schema.Types.Date,
    },
  },
  {
    timestamps: true,
  }
);

const Location = mongoose.model('location', locationSchema);

module.exports = Location;
