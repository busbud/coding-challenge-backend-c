import nconf from 'nconf'

export const DEFAULT_CONF = {
  APP_HOST: '0.0.0.0',
  APP_PORT: 8080,
  APP_STORE: 'redis' // 'memory' || 'redis'
}

export default nconf
  .env([
    'APP_HOST',
    'APP_PORT',
    'APP_STORE'
  ])
  .defaults(DEFAULT_CONF)
  