const mongoose = require('mongoose');

module.exports = async function (config) {
  return new Promise((resolve, reject) => {
    mongoose.connect(config.db);
    const db = mongoose.connection;
    db.on('error', err => {
      // console.error(err);
      reject(err);
    });
    db.once('open', () => {
      // console.log("database connection established");
      resolve(db);
    });
  });
};