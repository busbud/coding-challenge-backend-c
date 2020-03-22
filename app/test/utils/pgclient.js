const setup = require('../../properties');
const Client = require('pg').Client;
const client = new Client(setup.db);

const QUERY_TRUNCATE_TABLE = "TRUNCATE $1 CASCADE";
const QUERY_INSERT_MODEL = `
    INSERT INTO cities (id, name, ascii, alt_name, lat, long, feat_class, feat_code, country, cc2, admin1, admin2, admin3, admin4, population, elevation, dem, tz, modified_at) VALUES ($1, $2, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null) 
`;

module.exports = {
  trucate: async function(table) {
    try {
      return await client.query(QUERY_TRUNCATE_TABLE, [table]);
    } catch (err) {
      err.statusCode = 500;
      err.message = "Database operation failed with message: " + err.message;
      return setImmediate(() => {
        throw err;
      });
    }
  },
  insert: async function(model) {
    try {
      return await client.query(QUERY_TRUNCATE_TABLE, [table]);
    } catch (err) {
      err.statusCode = 500;
      err.message = "Database operation failed with message: " + err.message;
      return setImmediate(() => {
        throw err;
      });
    }
  }
};
