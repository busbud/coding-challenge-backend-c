const Geonames = require('geonames.js');

const COUNTRY_CODES = 'US, CA';
const CITIES_POPULATION_QUERY = 'cities5000';

module.exports = {
	getCities: async (name) => {
		const geonames = Geonames({
			username: process.env.USERNAME || 'badal_busbud',
			lan: 'en',
			encoding: 'JSON',
		});

		const result = await geonames.search({
			name_startsWith: name,
			country: COUNTRY_CODES,
			cities: CITIES_POPULATION_QUERY,
		});
		return result;
	},
};
