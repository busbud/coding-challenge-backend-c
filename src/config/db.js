require('dotenv').config();
const Sequelize = require('sequelize');

const {
  DATABASE_URL = 'suggestions_dev',
  DB_HOST,
  DB_USERNAME,
  DB_PASSWORD,
} = process.env;

module.exports = new Sequelize(DATABASE_URL, DB_USERNAME, DB_PASSWORD, {
  host: DB_HOST,
  operatorsAliases: false,
  dialect: 'postgres',
  dialectOptions: {
    ssl: process.env.DATABASE_URL ? true : false,
  },
  pool: {
    max: 5,
    min: 0,
    aquire: 30000,
    idle: 10000,
  },
});
