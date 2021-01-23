const defaultEnv = 'development';
const env = process.env.NODE_ENV || defaultEnv;

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
  },
  development: {},
  production: {},
};

const config = { ...configs.base, ...configs[env] };

module.exports = config;
