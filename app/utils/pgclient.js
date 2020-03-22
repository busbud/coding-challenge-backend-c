const setup = require('../properties')
const Pool = require('pg').Pool;
const pool = new Pool(setup.db);

module.exports = {
  select: async function (queryString, params) {
    try {
      const results = await pool.query(queryString, params);
      return results.rows;
    }
    catch (err) {
      err.statusCode = 500;
      err.message = 'Database operation failed with message: ' + err.message;
      return setImmediate(() => { throw err; });
    }
  }
}