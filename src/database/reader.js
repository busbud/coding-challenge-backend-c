const connect = require('camo').connect;
const path = require('path');

const databaseName = process.env.DATABASE_NAME || 'busbud_challenge';
const databasePath = path.join(path.resolve('./data/'), databaseName);
const uri = `nedb://${databasePath}`;

let database = null;

/**
 * Fetches the database handle through a Promise allowing other modules to query or write to
 * the database.
 */
function getDb() {
  return new Promise((resolve) => {
    if (database) {
      resolve(database);
    } else {
      connect(uri).then((db) => {
        database = db;
        resolve(database);
      });
    }
  });
}

export { getDb, databasePath };
