/**
 * Check query argument validity.
 *
 * @param value
 * @returns {{valid: boolean, errors}}
 */
function validateQuery(value) {
  let errors = {};
  if (value === '') {
    errors.q = 'You need a query in order to have suggestions !';
  }
  return {
    valid: Object.keys(errors).length === 0,
    errors
  };
}

/**
 * Check latitude and longitude arguments validity.
 *
 * @param latitude
 * @param longitude
 * @returns {{valid: boolean, errors}}
 */
function validateCoordinates (latitude, longitude) {
  const isLatitude = value => {
    return isFinite(value) && Math.abs(value) <= 90;
  };

  const isLongitude = value => {
    return isFinite(value) && Math.abs(value) <= 180;
  };

  let errors = {};
  if (!isLatitude(latitude)) {
    errors.latitude = 'The latitude is not correct';
  }
  if (!isLongitude(longitude)) {
    errors.longitude = 'The longitude is not correct';
  }
  return {
    valid: Object.keys(errors).length === 0,
    errors
  };
}

module.exports = {validateQuery, validateCoordinates};