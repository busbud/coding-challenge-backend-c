const sequelize = require('../config/db');
const { QueryTypes } = require('sequelize');

/**
 * Generates suggestions for the current request
 *
 * @param {string} searchParam
 * @param {string} latitude
 * @param {string} longitude
 *
 * @returns {Array<Object>}
 */
async function generateSuggestions(searchParam, latitude, longitude) {
  let distanceCalcQuery = '';
  const params = {
    searchParam,
    likeCondition: `${searchParam.toLowerCase()}%`,
  };

  // Calculate distance if latitude and longitude provided
  if (latitude && longitude) {
    distanceCalcQuery = `SQRT(POW(69.1 * (latitude::float -  :customLat::float), 2) +
      POW(69.1 * (:customLon::float - longitude::float) * COS(latitude::float / 57.3), 2)
      ) DESC, `;

    params.customLat = latitude;
    params.customLon = longitude;
  }

  // Run query to return matching results ordered by distance (if applicabale) and name matching
  const matchingGeonames = await sequelize.query(
    `SELECT CONCAT(name, ', ', state, ', ', country_code) as name, latitude, longitude,
    CAST(PERCENT_RANK() OVER (ORDER BY ${distanceCalcQuery} length(name) - length(:searchParam) DESC) as DECIMAL(10,1)) as score
    FROM public."geonames"
    WHERE (LOWER(name) LIKE :likeCondition OR LOWER(ascii_name) LIKE :likeCondition)
      AND population >= 5000
    ORDER BY score DESC`,
    {
      replacements: params,
      type: QueryTypes.SELECT,
    }
  );

  return matchingGeonames;
}

module.exports = { generateSuggestions };
