
/**
 * NPM modules
 */

const _ = require('lodash')

/**
 * Busbud modules
 */

const BusbudError = require('busbud-error')

/**
 * Application modules
 */

const config = require('../../config')
const services = require('../services')
const transforms = require('../transforms')



module.exports = (req, res) => {

	/**
	 * Let's say message is a mutable data object until lifecycle of the request, 
	 * it always stores data from query, body and etc...
	 */

	const message = req.query

	/**
	 * Prepare message
	 */

	message.q = _.escapeRegExp(message.q)
	message.latitude = message.latitude && parseFloat(message.latitude) || 90
	message.longitude = message.longitude && parseFloat(message.longitude) || 0
	message.population = message.population && parseInt(message.population) || config.settings.population.min
	message.countries = config.settings.countries.default

	/**
	 * Check message
	 */

	if (!message.q || _.size(message.q) > 64) {

		return res.send({
			error: new BusbudError('WRONG_Q_PARAM')
		})

	}

	if (!_.isNaN(message.latitude) && !_.isNaN(message.longitude) && (message.latitude < -90 || message.latitude > 90 || message.longitude < -180 || message.longitude > 180)) {

		return res.send({
			error: new BusbudError('WRONG_LATITUDE_OR_LONGITUDE_PARAM')
		})

	}

	/**
	 * Jump to the service
	 */

	services.suggestions(message, (error, suggestions) => {

		return res.send({
			error,
			suggestions: !error && transforms.suggestions(message, suggestions) || []
		})

	})

}


