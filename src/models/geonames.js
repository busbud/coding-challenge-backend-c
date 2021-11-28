const Sequelize = require('sequelize');
const db = require('../config/db');

module.exports = db.define('geonames', {
  geoname_id: {
    allowNull: false,
    autoIncrement: true,
    primaryKey: true,
    type: Sequelize.INTEGER,
  },
  name: {
    type: Sequelize.STRING(200),
  },
  country_code: {
    type: Sequelize.CHAR(2),
  },
  state: {
    type: Sequelize.CHAR(2),
  },
  latitude: {
    type: Sequelize.FLOAT,
  },
  longitude: {
    type: Sequelize.FLOAT,
  },
  population: {
    type: Sequelize.BIGINT,
  },
  createdAt: {
    allowNull: false,
    type: Sequelize.DATE,
  },
  updatedAt: {
    allowNull: false,
    type: Sequelize.DATE,
  },
});
