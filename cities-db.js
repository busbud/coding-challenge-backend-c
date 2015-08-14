var _ = require('underscore');
var es = require('event-stream');
var fs = require('fs');
var haversine = require('haversine');
var latinize = require('latinize');

// Country display names
var COUNTRY_MAP = {
  US: 'USA',
  CA: 'Canada'
};

// Map from Geonames admin1 code to Canadian province code
var PROVINCES_MAP = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '08': 'ON',
  '09': 'PE',
  '10': 'QC',
  '11': 'SK',
  '12': 'YT',
  '13': 'NT',
  '14': 'NU'
};

/*
 * CitiesDb constructor
 *
 * This is what this module exports.
 */
function CitiesDb(options) {
  this.options = _.defaults(options || {}, {
    distanceScoreWeight: 0.5
  });
  this._index = Object.create(null);
  this._fullNames = Object.create(null);
}

module.exports = CitiesDb;

/*
 * Read the data file at "path" into the database
 */
CitiesDb.prototype.readFile = function (path, done) {
  var seenFirstLine = false;
  fs.createReadStream(path, {encoding: 'utf8'})
    .pipe(es.split('\n'))
    .pipe(es.mapSync(function (line) {
      if (!seenFirstLine) {
        seenFirstLine = true;
        return;
      }
      this._parseLine(line);
    }.bind(this)))
    .pipe(es.wait(done));
};


/*
 * Get suggestions for a given query
 */
CitiesDb.prototype.getSuggestions = function (query, latitude, longitude) {

  // Shortcut if query is empty
  if (!query || query.length === 0) {
    return [];
  }

  // Obtain the list of suggestions from the db
  query = normalizeCityName(query);
  var cities = this._index[query];
  if (!cities) {
    return [];
  }

  // Score the suggestions
  var queryLength = query.length;
  var queryCoords = null;
  var distanceWeight = 0;
  var queryLengthWeight = 1.0;
  if (latitude && longitude) {
    queryCoords = {latitude: latitude, longitude: longitude};
    distanceWeight = this.options.distanceScoreWeight;
    queryLengthWeight = 1.0 - distanceWeight;
  }
  var suggestions = cities.map(function (info) {
    var score =
      queryLengthWeight * queryLengthScore(queryLength, info.name.length);
    if (queryCoords) {
      var distance = haversine(
        queryCoords,
        {latitude: info.latitude, longitude: info.longitude}
      );
      score += distanceWeight * distanceScore(distance);
    }
    return {
      name: info.fullName,
      latitude: info.latitude,
      longitude: info.longitude,
      score: score
    };
  }, this);
  suggestions.sort(function (a, b) {
    return b.score - a.score;
  });
  return suggestions;
};

/*
 * Parse a single line from the data file
 */
CitiesDb.prototype._parseLine = function (line) {

  // Read the fields
  var fields = line.split('\t');
  if (fields.length < 11) {
    return;
  }
  var name = fields[1];
  var latitude = Number(fields[4]);
  var longitude = Number(fields[5]);
  var countryCode = fields[8];
  var admin1 = fields[10];

  var country = COUNTRY_MAP[countryCode];
  var state = countryCode == 'CA' ? PROVINCES_MAP[admin1] : admin1;
  var fullName = name + ', ' + state + ', ' + country;

  // Validation: latitude / longitude
  if (isNaN(latitude) || isNaN(longitude)) {
    console.warn('Could not parse lat/long coordinates for city: ', fullName);
    return;
  }

  // Validation: avoid duplicates
  if (this._fullNames[fullName]) {
    console.warn('Duplicate city: ', fullName);
    return;
  }
  this._fullNames[fullName] = 1;

  // Store the city
  var key = normalizeCityName(fullName);
  var info = {
    name: name,
    fullName: fullName,
    latitude: latitude,
    longitude: longitude
  };
  this._add(key, info);
};

/*
 * Add a city to the database
 */
CitiesDb.prototype._add = function (key, info) {
  for (var i = 1; i <= key.length; i++) {
    var prefix = key.slice(0, i);
    var vals = this._index[prefix] || (this._index[prefix] = []);
    vals.push(info);
  }
};

/*
 * Normalize a query by removing accents, symbols and spaces
 */
function normalizeCityName(query) {
  return latinize(query).toLowerCase().replace(/[^a-z]/g, '');
}

/*
 * Query length score function
 *
 * This compares the query length with the length of the city name only.
 */
function queryLengthScore(queryLength, cityLength) {
  return Math.min(1.0, queryLength / cityLength);
}

/*
 * Distance score function
 *
 * Inverse square root of the distance, constrained between 0 and 1.
 */
function distanceScore(distance) {
  return 1.0 / (1 + Math.sqrt(distance / 1000));
}
