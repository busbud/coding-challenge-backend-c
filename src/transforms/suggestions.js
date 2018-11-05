

/**
 * NPM modules
 */

const _ = require('lodash')



module.exports = (message, suggestions) => {

	suggestions = _.map(suggestions, (suggestion) => _.pick(suggestion, ['name', 'latitude', 'longitude', 'score']))
	suggestions = _.sortBy(suggestions, (suggestion) => suggestion.score * -1)

	return suggestions

}


