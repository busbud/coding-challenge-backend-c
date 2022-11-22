/* 
 * This script is used to clean the data source
 * - Remove unneeded data
 * - Filter out cities under 5000 people
 * - Add province ascii
 * - Write cleaned data as JSON
 */

const readFileSync = require('fs').readFileSync;
const writeFileSync = require('fs').writeFileSync;

const desiredCountries = ["CA", "US"];
const resultPath = "./cities_canada-usa.json";
const nameIndex = 2;
const countryIndex = 8;
const stateIndex = 10;
const latitudeIndex = 4;
const longitudeIndex = 5;
const populationIndex = 14;

// Ref https://download.geonames.org/export/dump/admin1CodesASCII.txt
const provinceMap = {
	1: {full: "Alberta", short: "AB"},
	2: {full: "British Columbia", short: "BC"},
	3: {full: "Manitoba", short: "MB"},
	4: {full: "New Brunswick", short: "NB"},
	5: {full: "Newfoundland and Labrador", short: "NL"},
	6: {full: "", short: ""}, // Null
	7: {full: "Nova Scotia", short: "NS"},
	8: {full: "Ontairo", short: "ON"},
	9: {full: "Prince Edward Island", short: "PE"},
	10: {full: "Quebec", short: "QC"},
	11: {full: "Saskatchewan", short: "SK"},
	12: {full: "Yukon", short: "YT"},
	13: {full: "Northwest Territories", short: "NT"},
	14: {full: "Nunavut", short: "NU"},
}

try {
	const data = readFileSync("./cities_canada-usa.tsv", "utf8");
	const lines = data.split("\n");
	lines.shift(); // Remove header
	console.log(`Inital number of cities: ${lines.length}`)
	let cities = lines.map(line => {
		const data = line.split("\t");
		if (
			parseInt(data[populationIndex]) > 5000 && 
			desiredCountries.includes(data[countryIndex])
		) {
			let state;
			if (/^\d+$/.test(data[stateIndex])) {
				state = provinceMap[parseInt(data[stateIndex])].short;
			} else {
				state = data[stateIndex];
			}
			return {
				"name": `${data[nameIndex]}, ${state}, ${data[countryIndex]}`,
				"latitude": parseFloat(data[latitudeIndex]),
				"longitude": parseFloat(data[longitudeIndex]),
			}
		}
	})
	//Filter null values
	cities = cities.filter(city => !!city);
	console.log(`Resulting number of filtered cities: ${cities.length}`);
	console.log(`Cities written to ${resultPath}`);
	writeFileSync(resultPath, JSON.stringify(cities))
} catch (e) {
	console.error(e);
}
