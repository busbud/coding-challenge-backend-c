require('dotenv').config();
const Sequelize = require('sequelize');

const {
  DATABASE_URL = 'suggestions_dev',
  DB_HOST,
  DB_USERNAME,
  DB_PASSWORD,
} = process.env;
let sequelize;

if (process.env.DATABASE_URL) {
  sequelize = new Sequelize(DATABASE_URL, {
    dialect: 'postgres',
    dialectOptions: {
      ssl: true,
    },
  });
} else {
  sequelize = new Sequelize(DATABASE_URL, DB_USERNAME, DB_PASSWORD, {
    host: DB_HOST,
    dialect: 'postgres',
    pool: {
      max: 5,
      min: 0,
      aquire: 30000,
      idle: 10000,
    },
  });
}

module.exports = sequelize;
