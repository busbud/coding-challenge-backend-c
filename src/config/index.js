const dotenv = require('dotenv')
const dotenvParseVariables = require('dotenv-parse-variables')
const app = require('./app')
const cache = require('./cache')
const logger = require('./logger')
const searchEngine = require('./searchEngine')
const server = require('./server')

let env = dotenv.config()
if (env.error) throw env.error

env = dotenvParseVariables(process.env)

/**
 * Global configuration.
 * @module config
 */
module.exports = {
  app: app(env),
  cache: cache(env),
  logger: logger(env),
  searchEngine: searchEngine(env),
  server: server(env)
}
