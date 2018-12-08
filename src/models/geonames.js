'use strict';

const mongoose = require('mongoose');

// Schema
const GeonameSchema = new mongoose.Schema({
  admin1: {
    required: false,
    type: String
  },
  admin2: {
    required: false,
    type: String
  },
  admin3: {
    required: false,
    type: String
  },
  admin4: {
    required: false,
    type: String
  },
  alt_name: {
    required: false,
    type: String
  },
  ascii: {
    required: false,
    type: String
  },
  cc2: {
    required: false,
    type: String
  },
  country: {
    required: false,
    type: String
  },
  dem: {
    required: false,
    type: String
  },
  elevation: {
    required: false,
    type: Number
  },
  feat_class: {
    required: false,
    type: String
  },
  feat_code: {
    required: false,
    type: String
  },
  geoPosition: {
    required: true,
    sparse: true,
    type: [Number]
  },
  id: {
    required: false,
    type: Number,
    unique: 1
  },
  lat: {
    required: false,
    type: Number
  },
  long: {
    required: false,
    type: Number
  },
  modified_at: {
    required: false,
    type: Date
  },
  name: {
    required: false,
    type: String
  },
  population: {
    required: false,
    type: Number
  },
  tz: {
    required: false,
    type: String
  }
}, {
  timestamps: true
});

// Indexes
GeonameSchema.index({ '$**': 'text' });

module.exports = mongoose.model('Geoname', GeonameSchema);
