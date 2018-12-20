const Sequelize = require('sequelize');

const db = require('../db');


const City = db.define('city', {
  name: {
    type: Sequelize.STRING,
  },
  country: {
    type: Sequelize.STRING,
  },
  admin_division_code: {
    type: Sequelize.STRING,
  },
  lat: {
    type: Sequelize.STRING,
  },
  long: {
    type: Sequelize.STRING,
  },
}, {
  timestamps: false,
});

module.exports = City;
