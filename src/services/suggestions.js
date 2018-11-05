
/**
 * NPM modules
 */

const _ = require('lodash')
const async = require('async')

/**
 * Busbud modules
 */

const BusbudError = require('busbud-error')

/**
 * Application modules
 */

const dbs = require('../dbs')



module.exports = (message, callback) => {

	async.waterfall(
		[
			(callback) => getSuggestions(message, callback),
			(suggestions, callback) => applyScoring(message, suggestions, callback)
		],
		callback)

}

const getSuggestions = (message, callback) => {

	/**
	 * Should this query to be implemented on SQL side as PROCEDURE?
	 */

	dbs.pgsql.busbud_geonames.query(
		`
			SELECT
				name,
				latitude,
				longitude,
				population,
				country,
				((name ILIKE '${message.q}%')::INT) AS name_start_rank,
				(1 - (name <-> '${message.q}')) AS name_middle_rank,
				(ST_Distance(location, ST_MakePoint(${message.longitude}, ${message.latitude})::geography) / 1000)::INT AS distance
			FROM
				suggestions
			WHERE
				country IN(${message.countries.map((code) => `'${code}'`).join(',')}) AND population > ${message.population} AND name %> '${message.q}'
			ORDER BY
				name_start_rank DESC,
				name_middle_rank DESC,
				population DESC
			LIMIT
				5;
		`,
		(error, response) => {

			if (error) {

				return callback(new BusbudError('FAILED', error))

			} else if (!response.rows || !response.rows[0]) {

				return callback(null, [])

			} else {

				return callback(null, response.rows)

			}

		})

}

/**
 * Elementary scoring alghoritm.
 *
 * Note.
 * All params should be converting to the same scale - [0, 1]
 */

const applyScoring = (message, suggestions, callback) => {

	if (_.isEmpty(suggestions)) {

		return callback(null, suggestions)

	}

	const max_population = _.maxBy(suggestions, (suggestion) => suggestion.population).population
	const min_distance = _.minBy(suggestions, (suggestion) => suggestion.distance).distance
	const max_distance = _.maxBy(suggestions, (suggestion) => suggestion.distance).distance

	_.each(suggestions, (suggestion) => {

		suggestion.score = parseFloat(((
			suggestion.name_start_rank +
			suggestion.name_middle_rank +

			/**
			 * This is a simple logarithmic-scale method 
			 * which used to make smoother "sharps" in 
			 * the range of items based on "highest", 
			 * without losing an order of each item.
			 */

			Math.log(suggestion.population) / Math.log(max_population) +
			Math.log(min_distance) / Math.log(suggestion.distance)
		) / 4).toFixed(2))

	})

	return callback(null, suggestions)

}


