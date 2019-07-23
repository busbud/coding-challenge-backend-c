/**
 * Fetches and validates query parameters
 * @param   {Object}    query                                 Query Parameter
 * @return  {Object}    formatted_query                       Formatted & Validate query parameter
 * @return  {string}    formatted_query.q                     search term
 * @return  {Object}    formatted_query.coordinate            Coordinate Object
 * @return  {numeber}   formatted_query.coordinate.latitude   Coordinate's latitude
 * @return  {numeber}   formatted_query.coordinate.longitude  Coordinate's longitude
 * @return  {boolean}   formatted_query.is_valid              True if parameters are valid
 * @return  {string}    formatted_query.error_msg             Contains error message when fail validation
 */
function getSuggestionParameters(query) {
  var q = query.q;
  var latitude = parseFloat(query.latitude);
  var longitude = parseFloat(query.longitude);
  var is_valid = true;
  var error_msg = '';
  // verify query string
  if (typeof q === 'undefined' || (q.length === 0)) {
    error_msg = 'Invalid query parameter';
    is_valid = false;
  }
  // longitude must be present
  if (latitude && !longitude) {
    error_msg = 'Must provide a longitude';
    is_valid = false;
  }
  // latitude must be present
  if (!latitude && longitude) {
    error_msg = 'Must provide a latitude';
    is_valid = false;
  }
  // latitude must be valid
  if (latitude && ((latitude < -90) || (latitude > 90))) {
    error_msg = 'Invalid latitude';
    is_valid = false;
  }
  // longitude must be valid
  if (longitude && ((longitude < -180) || (longitude > 180))) {
    error_msg = 'Invalid longitude';
    is_valid = false;
  }
  return {
    q: q,
    coordinate: ((latitude && longitude) ? { latitude: latitude, longitude: longitude } : null),
    is_valid: is_valid,
    error_msg: error_msg
  };
}

/**
 * Formats a city to return in response
 * @param   {Object}  city            City Object
 * @param   {string}  city.name       City's display name
 * @param   {string}  city.state      City's State ISO code
 * @param   {string}  city.country    City's Coutry ISO code
 * @param   {string}  city.latitude   City's Latitude
 * @param   {string}  city.longitude  City's Longitude
 * @return  {Object}  city            Formated city object ready to display
 */
function serializeCity(city) {
  return ({
    name: [city.name, city.state, city.country].join(', '),
    latitude: city.lat,
    longitude: city.long,
    score: city.score
  });
}


module.exports = {
  getSuggestionParameters: getSuggestionParameters,
  serializeCity: serializeCity
};
