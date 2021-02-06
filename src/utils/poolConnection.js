/* eslint no-tabs: 0 */
const Sequelize = require('sequelize');
const constants = require('./constants');

const poolInstance = new Sequelize(constants.db.database, constants.db.username,
  constants.db.password, {
    host: constants.db.host,
    dialect: 'postgres',
    logging: false,
    sync: { force: false },
    pool: {
      max: 20,
      min: 5,
      idle: 10000,
    },
  });

poolInstance.sync().then(() => {
  console.log('DB connection sucessful.');
}, (err) => {
  console.log(err);
});

module.exports = poolInstance;
