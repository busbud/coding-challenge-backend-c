const Sequelize = require('sequelize');
const { db } = require('./db');

const City = db.define('city', {
  id: {
    type: Sequelize.INTEGER,
    primaryKey: true,
  },
  name: Sequelize.STRING(200),
  asciiName: {
    type: Sequelize.STRING(200),
    field: 'ascii',
  },
  alternateNames: {
    type: Sequelize.TEXT,
    field: 'alt_name',
  },
  latitude: {
    type: Sequelize.FLOAT,
    field: 'lat',
  },
  longitude: {
    type: Sequelize.FLOAT,
    field: 'long',
  },
  featureClass: {
    type: Sequelize.STRING(1),
    field: 'feat_class',
  },
  featureCode: {
    type: Sequelize.STRING(10),
    field: 'feat_code',
  },
  countryCode: {
    type: Sequelize.STRING(2),
    field: 'country',
  },
  cc2: Sequelize.STRING,
  admin1Code: {
    type: Sequelize.STRING(20),
    field: 'admin1',
  },
  admin2Code: {
    type: Sequelize.STRING(80),
    field: 'admin2',
  },
  admin3Code: {
    type: Sequelize.STRING(20),
    field: 'admin3',
  },
  admin4Code: {
    type: Sequelize.STRING(20),
    field: 'admin4',
  },
  population: Sequelize.INTEGER,
  elevation: Sequelize.INTEGER,
  dem: Sequelize.INTEGER,
  timezone: {
    type: Sequelize.STRING(40),
    field: 'tz',
  },
  modificationDate: {
    type: Sequelize.DATE,
    field: 'modified_at',
  },
  geom: Sequelize.GEOMETRY('Point', 4326),
}, { timestamps: false });

module.exports = { City };
