import nconf from 'nconf'

export const DEFAULT_CONF = {
  PORT: 8080,
  REDIS_URL: process.env.REDIS_PORT
}

export default nconf
  .env([
    'PORT',
    'REDIS_URL'
  ])
  .defaults(DEFAULT_CONF)
