/**
 * To normalize (cast, reformat, etc..) params
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Convert to a Date object from a mysql date or mysql datetime format
 * 
 * @param  {String} value
 * @return {Date}
 */
function mySQLDate(value) {
  return di.moment(value, 'YYYY-MM-DD HH:mm:ss').toDate();
}

/**
 * Normalize a name
 * 
 * - Lowercase
 * - Remove any characters that are not either a letter or space
 * - Remove duplicate sapces
 * - Remove prefix and suffix white spaces
 * 
 * @param  {String} value
 * @return {String}
 */
function name(value) {

  // Lowercase
  value = value.toLocaleLowerCase();

  // Remove any characters that are not either a letter or space
  value = value.replace(/[^a-z ]/g, ' ');

  // Remove duplicate sapces
  value = value.replace(/ {2,}/g, ' ');

  // Remove prefix and suffix white spaces
  value = di.lodash.trim(value);

  return value;

}

////////////////////////////////////////////////////
// Module //////////////////////////////////////////
////////////////////////////////////////////////////

module.exports = {
  mySQLDate: mySQLDate,
  name: name
};
