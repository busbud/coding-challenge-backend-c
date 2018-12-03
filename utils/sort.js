const { curry } = require("lodash/fp");
/**
 * sort a an array of object by a numerical field inside
 * @param {'ASC' | 'DSC'} type
 * @param {string} field
 * @param {Array<Object>} predicates
 */
const sortByNumericalField = curry((type, field, predicates) => {
  return predicates.slice().sort((a, b) => {
    const aValue = a[field];
    const bValue = b[field];
    switch (type) {
      case "ASC":
        return aValue - bValue;
      case "DSC":
        return bValue - aValue;
      default:
        return 0;
    }
  });
});

module.exports = {
  sortByNumericalField
};
