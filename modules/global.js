/**
 * Percentages of preferences in distance vs name
 * WEIGHT_NAME must be a value between 0 and 1 inclusive
 * represent the percentage of importance you give to the matching 'name'
 * the difference is the remaining perentage of importance given to short 'distance'
 */
const WEIGHT_NAME = 0.15; //15%

module.exports.WEIGHT_NAME = WEIGHT_NAME; //importance of precision in name
module.exports.WEIGHT_DISTANCE = 1 - WEIGHT_NAME; //importance given to short distance locations
module.exports.NUM_DECIMALS = 2; //number of decimals shown in the score

module.exports.GOOGLE_MAPS_KEY = 'AIzaSyAsap645uYiwbZh_cfm-LEp1rdRGB4nKXU'; //key to access to Google maps API
module.exports.TSV_PATH = './data/cities_canada-usa.tsv'; //name of the file containing the data of the locations
module.exports.MAX_POPULATION = 5000; //give preferences to locations where population is equals and greater than this number

module.exports.MIN_LENGHT_SEARCH = 3; //minimum longitud of search string
