/**
 * Location model. This would be your ORM model. Normally would use Mongoose or Sequelize.
 * In this case it's using the mock DB file streamer as I think setting up a DB is out of scope for this exercise.
 */

// Schema for the table. 
const LOCATION_SCHEMA = {
  id: 'String',
  name: 'String',
  asciiname: 'String',
  alternatenames: 'String',
  latitude: 'Decimal',
  longitude: 'Decimal',
  feature_class: 'String',
  feature_code: 'String',
  country_code: 'String',
  cc2: 'String',
  admin1_code: 'String',
  admin2_code: 'String',
  admin3_code: 'String',
  admin4_code: 'String',
  population: 'Integer',
  elevation: 'Integer',
  dem: 'Integer',
  timezone: 'String',
  modification_date: 'String'
}


/**
 * Take the raw db data and casts it into a location object. This would normally be done by whatever ORM
 * you are using. Mongoose, Sequelize, etc.
 * @param {String} rawDBData raw data from the data source
 * @returns 
 */
module.exports = function castToLocationObject(rawDBData) {
  const tokenisedString = rawDBData.split('\t');
  return Object.keys(LOCATION_SCHEMA).reduce((acc, key, index) => {
    const dataType = LOCATION_SCHEMA[key];

    if (!tokenisedString[index]) {
      return acc;
    }

    if (dataType == 'Integer') {
      acc[key] = parseInt(tokenisedString[index]);
      return acc;
    }

    if (dataType == 'Decimal') {
      acc[key] = parseFloat(tokenisedString[index]);
      return acc;
    }

    acc[key] = tokenisedString[index];
    return acc;
  }, {});
}
