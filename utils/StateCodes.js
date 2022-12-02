// https://download.geonames.org/export/dump/admin1CodesASCII.txt

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

const countryCodeMap = {
  'CA': 'CANADA',
  'US': 'USA'
};

const getStateCode = (code) => {
  return provinceMap[+code] ? provinceMap[+code] : code;
};

module.exports = { getStateCode, countryCodeMap };