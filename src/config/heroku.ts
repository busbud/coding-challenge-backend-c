import dotenv from 'dotenv';

dotenv.config();

export default {
  dyno: Boolean(process.env.DYNO),
  appPort: Number(process.env.PORT),
  postgresUrl: process.env.DATABASE_URL,
  redisUrl: process.env.REDIS_TLS_URL,
};
