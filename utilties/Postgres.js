/* eslint-disable object-curly-newline */
require('dotenv').config();

const { Pool } = require('pg');

const user = process.env.DATABASE_USER;
const host = process.env.DATABASE_HOST;
const database = process.env.DATABASE_DB;
const password = process.env.DATABASE_PASS;
const port = process.env.DATABASE_PORT;

const pool = new Pool({ user, host, database, password, port });

module.exports = {
  query: (text, params) => pool.query(text, params),
};
