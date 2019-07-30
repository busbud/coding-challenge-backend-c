const MongoClient = require("mongodb").MongoClient;
const { DATABASE_URL, MONGODB_DATABASE } = process.env;
const client = new MongoClient(DATABASE_URL, { useNewUrlParser: true });
let db;

/**
 * Connect to mongodb and make the client accessible through exports.mongo
 */
const connect = () => {
  client.connect(err => {
    if (err) {
      console.log(err);
    } else {
      db = client.db(MONGODB_DATABASE);
      console.log("Connected to mongo db");
    }
  });
};

/**
 * @returns {Db}
 */
const mongo = () => {
  return db;
};

/**
 * @returns {Boolean}
 */
const isConnectedToMongo = () => {
  return !!db && !!db.topology && db.topology.isConnected();
};

const getClient = () => {
  return client;
};

module.exports = {
  connect,
  mongo,
  isConnectedToMongo,
  getClient
};
