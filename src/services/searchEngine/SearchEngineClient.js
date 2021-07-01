const EventEmitter = require('events')
const { Client } = require('@elastic/elasticsearch')
const retry = require('retry')

/**
 * @class
 * @extends EventEmitter
 * @description Search engine client (wrapper of elastic search client)
 * @param {Object} services
 * @param {Object} services.logger - A logger service
 * @return {GracefulShutdown}
 */
class SearchEngineClient extends EventEmitter {
  /**
   * @constructor
   * @description create a search engine client
   * @param {Object} options
   * @param {string} [options.nodes='http://localhost:9000']
   * @param {number} [options.maxRetries=5]
   * @param {number} [options.requestTimeout=60000]
   * @param {Object} [options.auth]
   * @param {string|null} [options.auth.type=null]
   * @param {string} [options.auth.username]
   * @param {string} [options.auth.password]
   * @param {string} [options.auth.apiKey]
   * @param {Connection} [options.connection]
   */
  constructor ({
    nodes = 'http://localhost:9000',
    maxRetries = 5,
    requestTimeout = 60000,
    auth = {
      type: null
    },
    connection
  } = {}) {
    super()
    this.nodes = nodes
    this.maxRetries = maxRetries
    this.requestTimeout = requestTimeout
    this.isConnected = false
    this.repositories = new Map()
    this.connection = connection

    // Retry with exponential backoff strategy
    this.operation = retry.operation({
      retries: this.maxRetries
    })
    this.rawClient = null

    if (auth && auth.type === 'basic') {
      this.auth = {
        username: auth.username,
        password: auth.password
      }
    } else if (auth && auth.type === 'apiKey') {
      this.auth = {
        apiKey: auth.apiKey
      }
    } else {
      this.auth = null
    }
  }

  /**
   * @async
   * @function connect
   * @description connect the client to the cluster
   */
  async connect () {
    if (this.isConnected) {
      return
    }
    this.emit('connecting')
    const options = {
      nodes: this.nodes,
      maxRetries: this.maxRetries,
      requestTimeout: this.requestTimeout
    }

    if (this.auth) {
      options.auth = this.auth
    }

    if (this.connection) {
      options.Connection = this.connection
    }
    this.rawClient = new Client(options)

    await new Promise((resolve, reject) => {
      this.operation.attempt(async () => {
        try {
          await this.healthCheck()
          this.isConnected = true
          this.emit('connected')
          resolve()
        } catch (err) {
          if (this.operation.retry(err)) {
            this.emit('retry')
            return
          }
          reject(this.operation.mainError())
        }
      })
    })

    this.isConnected = true
  }

  /**
   * @async
   * @function close
   * @description close the connection
   */
  async close () {
    this.operation.stop()

    if (this.isConnected) {
      await this.rawClient.close()
      this.emit('closed')
    }
  }

  /**
   * @async
   * @function healthCheck
   * @description Check the health of the cluster
   * @returns {healthStatus}
   */
  async healthCheck () {
    const { body } = await this.rawClient.cluster.health()

    /**
     * @typedef {Object} healthStatus
     * @property {string} clusterName
     * @property {('green'|'yellow'|'red')} status
     */
    return {
      clusterName: body.cluster_name,
      status: body.status
    }
  }

  /**
   * @callback BuilderSearchEngineRepository
   * @param {Client} client
   * @returns {Object}
   **/
  /**
   * @function addRepository
   * @description add a repository
   * @param {string} name
   * @param {BuilderSearchEngineRepository} repositoryBuilder
   */
  addRepository (name, repositoryBuilder) {
    if (!this.isConnected) {
      throw new Error(
        `You must connect to ${this.node} the before add repository`)
    }
    this.repositories.set(name, repositoryBuilder(this.rawClient))
  }

  /**
   * @function getRepository
   * @description Get a repository by its name
   * @param {string} name
   * @returns {Object}
   */
  getRepository (name) {
    if (!this.isConnected) {
      throw new Error(
        `You must connect to ${this.node} the before get repository`)
    }
    return this.repositories.get(name)
  }

  /**
   * @callback Migration
   * @param {Client} client
   * @returns {Promise}
   **/
  /**
   * @async
   * @function applyMigration
   * @description Apply a migration
   * @param {Migration} migration
   */
  async applyMigration (migration) {
    return migration(this.rawClient)
  }
}

module.exports = SearchEngineClient
