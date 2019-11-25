/* eslint-disable no-tabs */
require('dotenv').config();

const { Client } = require('pg');
const fs = require('fs');

const sql = fs.readFileSync('./data/busbud.sql').toString();

const client = new Client({
  user: process.env.DATABASE_USER,
  password: process.env.DATABASE_PASS,
  database: process.env.DATABASE_DB,
  port: process.env.DATABASE_PORT,
  host: process.env.DATABASE_HOST,
});

try {
  client.connect();
} catch (e) {
  console.log(e);
}

client.query(sql, (err) => {
  if (err) {
    console.log('Seeding failed: ', err);
    process.exit(1);
  }

  console.log('Seeding successful.');

  client.end();
  process.exit(0);
});
