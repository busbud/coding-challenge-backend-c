const Sequelize = require('sequelize');

let db;
const options = {
  dialect: 'postgres',
  operatorsAliases: false,
  logging: false,
};

if (process.env.DATABASE_URL) {
  db = new Sequelize(process.env.DATABASE_URL, options);
} else {
  db = new Sequelize({
    ...options,
    database: process.env.DB_NAME || 'postgres',
    username: process.env.DB_USER || 'postgres',
    password: process.env.DB_PASS || null,
    host: process.env.DB_HOST || 'localhost',
  });
}

module.exports = { db };
