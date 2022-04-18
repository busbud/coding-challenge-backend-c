const dotenv = require('dotenv-flow');

const envFound = dotenv.config();

if (envFound.error) {
  throw new Error("Couldn't find .env file");
}

module.exports = {
  development: {
    url: process.env.DATABASE_URL,
    dialect: 'postgres'
  },
  test: {
    url: process.env.DATABASE_URL,
    dialect: 'postgres'
  }
};
