const fs = require("fs");
const { curry } = require("lodash/fp");
const { first, last, omit } = require("lodash");
const { numericalCoordinate, distanceBetween } = require("../geo");
const { levenshteinDistance, sanitizeString } = require("../text");
const { sortByNumericalField } = require("../sort");
const { twoDecimalsAtMost } = require("../numbers");

/** load the index in memory for the sake of example */
function loadIndex(file) {
  const content = fs.readFileSync(file);
  return JSON.parse(fs.readFileSync(file, "utf-8"));
}

/**
 *
 * @param {*} results
 */
function withLevensteinDistanceScore(reference, results) {
  return results.map(result => ({
    ...result,
    /**
     * the levenstein distance gets the number of differents char from a reference
     */
    score: twoDecimalsAtMost(
      1 - levenshteinDistance(reference, result.onlyName) / reference.length
    )
  }));
}

/**
 * weigh similar results depending on the distance to a given point
 * the scores must in order to produce a scale
 * @param {{latitude: number, longitude: number}} reference
 * @param {Array<any>} results
 */
function weighWithGeoDistanceScore(reference, results, geoPriority = 2) {
  // enrich with distance
  let withDistance = results.map(result => ({
    ...result,
    distanceFromPivot: distanceBetween(reference, {
      latitude: result.latitude,
      longitude: result.longitude
    })
  }));

  // sort by distance from pivot
  withDistance = sortByNumericalField("ASC", "distanceFromPivot", withDistance);

  // get closests and furthest to build a liner scalre
  const closestDistance = first(withDistance).distanceFromPivot;
  const furthestDistance = last(withDistance).distanceFromPivot;
  // weigh the geolocation according the heuristic priority given as an input@
  return withDistance.map(result => {
    // linear scale here !
    const geoScore = 1 - result.distanceFromPivot / furthestDistance;
    // barycenter of geoScore and actual score, rounded to 2 decimals when necessary
    const newScore = twoDecimalsAtMost(
      (geoPriority * geoScore + result.score) / (geoPriority + 2)
    );
    return {
      ...omit(result, "distanceFromPivot"),
      // weigh  geolocation 2x more than text accuracy
      score: newScore
    };
  });
}

/**
 * generic suggest method, find matches, and weigh them by levenstein distance and geo distance
 */
const suggest = curry((matchesFinder, db, query, pivot = null) => {
  if (query !== "") {
    // sanitized version of the query
    let matches = matchesFinder(db, query);
    matches = withLevensteinDistanceScore(query, matches);

    // weight by geolocation
    if (pivot) {
      matches = weighWithGeoDistanceScore(pivot, matches);
    }
    // sort by score
    matches = sortByNumericalField("DSC", "score", matches);
    return {
      suggestions: matches
    };
  } else {
    return { suggestions: [] };
  }
});

/** utility function to validate incoming get params in the suggestions endpoint */
function validateParamsMiddleware(req) {
  const [query, latitude, longitude] = [
    req.query.q,
    req.query.latitude,
    req.query.longitude
  ];
  const hasQuery = query && query !== "";
  const hasLatitude = !isNaN(parseFloat(latitude));
  const hasLongitude = !isNaN(parseFloat(longitude));

  const pivotIsDefined = hasLatitude || hasLongitude;
  return hasQuery
    ? {
        query,
        ...(pivotIsDefined
          ? {
              pivot: {
                latitude,
                longitude
              }
            }
          : {})
      }
    : null;
}
module.exports = {
  loadIndex,
  suggest,
  withLevensteinDistanceScore,
  validateParamsMiddleware
};
