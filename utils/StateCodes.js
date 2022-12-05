// this took a while to figure out..
// Looks like for US values the admin1 maps to an actual state code
// but for Canadian provinces we need to map admin1 code to a province code
// this list was found here: https://download.geonames.org/export/dump/admin1CodesASCII.txt

const { COUNTRY_CODES } = require('../constants');

const provinceMap = {
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
  13: "NT"
};

// map the country code to a country name
const countryCodeMap = {
  [COUNTRY_CODES.CA]: 'CANADA',
  [COUNTRY_CODES.CA]: 'USA'
};

const getStateCode = (code) => {
  return provinceMap[+code] ? provinceMap[+code] : code;
};

module.exports = {
  getStateCode,
  countryCodeMap
};
