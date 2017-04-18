'use strict'

/**
 * Use the country code and the admin1 code to retrieve the 2-characters
 * state code according to http://download.geonames.org/export/dump/admin1CodesASCII.txt
 * and https://en.wikipedia.org/wiki/Canadian_postal_abbreviations_for_provinces_and_territories.
 * For USA, the state is actually admin1 value.
 * For Canada, the state is fetched in a static array.
 * For other countries, a different process might be necessary.
 */ 
function getStateCode(admin1, country) {
	if (country === 'US') {
		return admin1;
	}
	let canadianProvinces = {
		1: "AB",
		2: "BC",
		3: "MB",
		4: "NB",
		5: "NL",
		7: "NS",
		8: "ON",
		9: "PE",
		10: "QC",
		11: "SK",
		12: "YT",
		13: "NT",
		14: "NU",
	};
	return canadianProvinces[admin1];
}

/**
 * Use the country code to get the full country name.
 * This app considers only USA and Canada, we can limit the computation time.
 * However, a more complex process could be implemented here to include more countries.
 */ 
function getCountryName(country) {
	return (country === 'US') ? 'USA' : 'Canada';
}

module.exports = {
	getStateCode,
	getCountryName
}
