import nconf from 'nconf'

export const DEFAULT_CONF = {
  APP_PORT: 8080
}

export default nconf
  .env([
    'PORT',
    'REDIS_URL'
  ])
  .defaults(DEFAULT_CONF)
