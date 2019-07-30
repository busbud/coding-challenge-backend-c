const map = {
  CA: {
    "01": "AB",
    "02": "BC",
    "03": "MB",
    "04": "NB",
    "13": "NT",
    "07": "NS",
    "14": "NU",
    "08": "ON",
    "09": "PE",
    "10": "QC",
    "11": "SK",
    "12": "YT",
    "05": "NL"
  }
};

/**
 * Translate the numerical admin1 code to alpabetical code.
 *
 * @param {string} country
 * @param {string} admin1
 */
const stateCodeAbbrv = (country, admin1) => {
  let code = admin1;
  if (map[country]) {
    code = map[country][admin1] || admin1;
  }
  return code;
};

module.exports = {
  stateCodeAbbrv
};
