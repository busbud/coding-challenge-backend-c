
const config = require('../config')

/**
 * NPM modules
 */

const pg = require('pg')
const express = require('express')

/**
 * Busbud modules
 */

const log = require('busbud-log')

/**
 * Application modules
 */

const dbs = require('./dbs')
const controllers = require('./controllers')



module.exports = () => {

	/**
	 * Connect to databases
	 */

	dbs.pgsql.busbud_geonames = new pg.Pool(config.pgsql.busbud_geonames)

	/**
	 * Start server
	 */

	const app = express()

	app
		.get('/suggestions', (req, res) => controllers.suggestions(req, res))

	app
		.listen(config.server.port)
		.on('error', (error) => log.fail('EXPRESS HTTP x'))
		.on('listening', (error) => log.info('EXPRESS HTTP ✔'))

}


