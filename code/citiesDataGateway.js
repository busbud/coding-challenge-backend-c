const fs = require('fs');
const d3 = require('d3');

module.exports = class citiesDataGateway {

	constructor() {
		const data = fs.readFileSync('./data/cities_canada-usa.tsv', "utf8");
		this.data = d3.tsvParse(data);
	}

	all() {
		// we use the JSON trick to be sure that the consumers
		// of the method will never alter the in memory data
		return JSON.parse(JSON.stringify(this.data));
	}
}
