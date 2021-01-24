const defaultEnv = 'development';
const env = process.env.NODE_ENV || defaultEnv;
const crypto = require('crypto');

const merge = (target, source) => {
  // Iterate through `source` properties and if an `Object` set property
  // to merge of `target` and `source` properties
  // eslint-disable-next-line no-restricted-syntax
  for (const key of Object.keys(source)) {
    if (source[key] instanceof Object) Object.assign(source[key], merge(target[key], source[key]));
  }

  // Join `target` and modified `source`
  Object.assign(target || {}, source);
  return target;
};

const configs = {
  base: {
    env,
    hostname: process.env.HOST || '127.0.0.1',
    port: process.env.PORT || 2345,
    elasticSearch: {
      baseUrl: process.env.ELASTICSEARCH_BASEURL || 'http://localhost:9200',
      index: 'suggestions_index',
    },
    suggestionDataSource: `${process.cwd()}/data/cities_canada-usa.tsv`,
    maintenanceToken: process.env.MAINTANANCE_TOKEN || crypto.randomBytes(64).toString('hex').slice(0, 64),
  },
  development: {
    maintenanceToken: 'dev',
  },
  production: {
    elasticSearch: {
      agent: {
        maxSockets: 2,
        maxFreeSockets: 2,
      },
    },
  },
};

const config = merge(configs.base, configs[env]);

console.log(env);
console.log(config);

module.exports = config;
