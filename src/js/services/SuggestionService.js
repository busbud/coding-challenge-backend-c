import mongodb, { MongoClient } from "mongodb";
import es6promise from "es6-promise";
import natural from "natural";
es6promise.polyfill();

export default class SuggestionService {

  constructor(locationRepo) {
    this.locationRepo = locationRepo;
  }

  /**
   * Return a list of suggestions based on a query and possible location.
   *
   * Fetches all locations starting with or matching the query. Then it calculates the similarity between the
   * query and each location's name
   * using the Jaro-Winkler distance, which returns a score between 0 and 1 and is supposed to be a slightly
   * faster algorithm than Levenshtein. Finally, if the location was provided, and since the results will be
   * ordered by proxmity by the database, we will apply a "closeness-factor" based on the locations' order in
   * the results so that we weight closer results higher up.
   *
   * @param query the partial or complete city name to search for
   * @param location an optional object containing latitude and longitude
   */
  listSuggestions(query, location, limit) {
    const fLocations = this.locationRepo.listLocations(query, location);
    return fLocations.then((locations) => {
      const numLocations = locations.length * 1.0;
      return locations
        .map(SuggestionService._generateScore)
        .filter((suggestion) => {
          return suggestion.score > 0.25
        })
        .sort((a, b) => {
          if (a.score > b.score) return -1;
          else if (b.score > a.score) return 1;
          else return 0;
        })
        .slice(0, limit);
    });
  }

  static _generateScore(suggestion, idx) {
    /* Generate this suggestion's initial score. */
    const queryParts = query.split(',');
    const suggestParts = suggestion.fullName.split(',');

    /* Score each part of the query. Can be useful if we need to search by province and country
     * names in the future, ie: Montreal, Quebec, Canada will be treated as montreal,quebec,canada and
     * we will compare each segment for similarity separately. */
    let score = queryParts.map((part, idx) => {
      if (typeof suggestParts[idx] != "undefined") {
        return natural.JaroWinklerDistance(part, suggestParts[idx]);
      }
    }).reduce((accum, elem) => { return accum + elem; });

    if (queryParts.length > 1) {
      score = score * 1.0 / queryParts.length;
    }

    /* If location is given, mongodb will return the results sorted by proximity. Transform this
     * ordering into a closeness-factor so that we can reduce the overall score as distance increases. */
    if (typeof location != "undefined" && location != null) {
      let closenessFactor = (numLocations - (idx/2 * 1.0)) / numLocations;
      if (closenessFactor > 1) closenessFactor = 1;
      else if (closenessFactor <= 0) closenessFactor = 0;
      score = score * closenessFactor;
    }
    /* Penalize results that aren't exact matches */
    else if (queryParts[0] != suggestParts[0]) {
      score = score * 0.75;
    }

    suggestion.score = score;
    return suggestion;
  }
}
