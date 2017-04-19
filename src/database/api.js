const City = require('../models/city').default;
const getDb = require('./reader').getDb;


/**
 * Queries the database for all Cities with a name starting with the given string.
 *
 * Further filtering is applied inside the City model's module.
 */
export default function getCitiesByNameStartingWith(nameQuery) {
  return new Promise((resolve) => {
    getDb().then(() => {
      City.findByNameStartingWith(nameQuery).then((cities) => {
        resolve(cities);
      });
    });
  });
}
