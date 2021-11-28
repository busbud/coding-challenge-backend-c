require('dotenv').config();
const Sequelize = require('sequelize');

const { DB_HOST, DB_USERNAME, DB_PASSWORD } = process.env;

module.exports = new Sequelize('suggestions_dev', DB_USERNAME, DB_PASSWORD, {
  host: DB_HOST,
  dialect: 'postgres',
  operatorsAliases: false,
  pool: {
    max: 5,
    min: 0,
    aquire: 30000,
    idle: 10000,
  },
});
