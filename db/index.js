require('dotenv').config();
const Sequelize = require('sequelize');


const db = new Sequelize(process.env.DATABASE_URL, {
  logging: false,
  operatorsAliases: false,
});

module.exports = db;
