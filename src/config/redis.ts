import dotenv from 'dotenv';
import herokuConfig from 'config/heroku';

dotenv.config();

export default herokuConfig.dyno
  ? {
    url: herokuConfig.redisUrl,
    tls: { rejectUnauthorized: false },
  }
  : {
    host: process.env.REDIS_HOST,
    port: Number(process.env.REDIS_PORT),
    username: process.env.REDIS_USER,
    password: process.env.REDIS_PASSWORD,
    tls: Number(process.env.REDIS_TLS)
      ? { rejectUnauthorized: Boolean(Number(process.env.REDIS_TLS_VERIFY_CERT)) }
      : undefined,
  };
