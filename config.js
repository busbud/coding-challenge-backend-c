const defaultEnv = 'development';
const env = process.env.NODE_ENV || defaultEnv;
const crypto = require('crypto');

console.log(crypto.randomBytes(64).toString('hex').slice(0, 64));

const configs = {
  base: {
    env,
    hostname: '127.0.0.1',
    port: 2345,
    elasticSearch: {
      baseUrl: 'http://localhost:9200',
      index: 'suggestions_index',
    },
    suggestionDataSource: `${process.cwd()}/data/cities_canada-usa.tsv`,
    maintenanceToken: crypto.randomBytes(64).toString('hex').slice(0, 64),
  },
  development: {
    maintenanceToken: 'dev',
  },
  production: {},
};

const config = { ...configs.base, ...configs[env] };

module.exports = config;
