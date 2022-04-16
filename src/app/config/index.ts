import dotenv from 'dotenv-flow';

const envFound = dotenv.config();

if (envFound.error) {
  throw new Error("Couldn't find .env file");
}

export default {
  /**
   * API configs
   */
  api: {
    port: process.env.API_PORT,
    hostname: process.env.API_HOSTNAME,
  },
  /**
   * Database settings
   */
  database: {
    url: process.env.DATABASE_URL
  }
};
