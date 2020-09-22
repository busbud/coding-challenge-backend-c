import pg from 'pg';

const { Pool } = pg;

const pool = new Pool({
  host: 'localhost',
  database: 'suggestions-service',
  user: 'docker',
  password: 'docker',
  max: 10,
  idleTimeoutMillis: 30000,
  connectionTimeoutMillis: 2000,
});

const executeQuery = async (query, values) => {
  const client = await pool.connect();
  let result;
  try {
    result = await client.query(query, values);
  } finally {
    client.release();
  }
  return result.rows;
};

export default {
  executeQuery,
};
