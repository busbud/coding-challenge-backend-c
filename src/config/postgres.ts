import dotenv from 'dotenv';
import herokuConfig from 'config/heroku';

dotenv.config();

export default herokuConfig.dyno
  ? {
    connectionString: herokuConfig.postgresUrl,
    ssl: { rejectUnauthorized: false },
  }
  : {
    host: process.env.POSTGRES_HOST,
    port: Number(process.env.POSTGRES_PORT),
    user: process.env.POSTGRES_USER,
    password: process.env.POSTGRES_PASSWORD,
    database: process.env.POSTGRES_DATABASE,
    ssl: Number(process.env.POSTGRES_TLS)
      ? { rejectUnauthorized: Boolean(Number(process.env.POSTGRES_TLS_VERIFY_CERT)) }
      : undefined,
  };
