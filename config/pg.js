import pg from 'pg';

const { Pool } = pg;

const connectionString = process.env.DATABASE_URL || 'postgres://docker:docker@localhost:5432/suggestions-service';

const pool = new Pool({
  connectionString,
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
