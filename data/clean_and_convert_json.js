/* 
 * This script is used to clean the data source
 * - Remove unneeded data
 * - Filter out cities under 5000 people
 * - Add province ascii
 * - Write cleaned data as JSON
 */

import {readFileSync, writeFileSync} from 'fs';

const desiredAttributes = ["name", "ascii", "lat", "long", "country", "population", "admin1"];
const desiredCountries = ["CA", "US"];
const resultPath = "./cities_canada-usa.json";
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
	console.log(`Inital number of cities: ${lines.length}`)
	const attributes = lines.shift().split("\t");
	const cities = lines.map(line => {
		const data = line.split("\t");
		return attributes.reduce((obj, attribute, index) => {
			if (desiredAttributes.includes(attribute)) {
				if (attribute === "admin1") {
					if (/^\d+$/.test(data[index])) {
						obj["state"] = provinceMap[parseInt(data[index])].short;
					} else {
						obj["state"] = data[index];
					}
				} else {
					obj[attribute] = data[index];
				}
			}
			return obj;
		}, {});
	})
	const big_cities = cities.filter(city => city.population >= 5000 & desiredCountries.includes(city.country));
	console.log(`Resulting number of filtered cities: ${big_cities.length}`);
	console.log(big_cities);
	writeFileSync(resultPath, JSON.stringify(big_cities));
} catch (e) {
	console.error(e);
}
