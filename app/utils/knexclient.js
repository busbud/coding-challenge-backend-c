var setup = require('../properties');
var knex = require('knex')({
  client: 'pg',
  connection: setup.db,
  pool: { min: 2, max: 10 }
});

module.exports = knex;
