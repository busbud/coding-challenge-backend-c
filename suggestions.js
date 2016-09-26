var fs = require('fs');
var csvParse = require('csv-parse/lib/sync');
var Heap = require('heap');


var MIN_QUERY_LENGTH = 2;
var SUGGESTIONS_LIMIT = 10;
var SCORING_DIST_THRESHOLD = 30; // Degrees, approximately 2 timezones

var COUNTRY_ISO_TO_NAME = {
  // Restricted to US and Canada only for now.
  "US": "USA",
  "CA": "Canada"
};

var CA_ADMIN_CODE_TO_PROVINCE = {
  // Source: http://download.geonames.org/export/dump/admin1CodesASCII.txt
  // The US codes map directly to state name. Restricted to US and Canada
  // only, so just embed it here instead of reading/parse the source file.
  "01": "AB",
  "02": "BC",
  "03": "MB",
  "04": "NB",
  "13": "NT",
  "07": "NS",
  "14": "NU",
  "08": "ON",
  "09": "PE",
  "10": "QC",
  "11": "SK",
  "12": "YT",
  "05": "NL"
};

var cities = [];
var citiesByFirstChar = { };
var maxPopulation = 0;

function init() {
  // Parse the data file to populate cities, and this is done synchronously during
  // initialization since we want the data to be ready before we start serving
  var input = fs.readFileSync(__dirname+'/data/cities_canada-usa.tsv');
  var data = csvParse(input, { 
    columns: true,
    delimiter: '\t',
    quote: '\u0000',
    skip_empty_lines: true
  });

  for (var i = 0; i < data.length; i++) {
    var city = {
      name: data[i].name,
      population: parseInt(data[i].population),
      latitude: parseFloat(data[i].lat),
      longitude: parseFloat(data[i].long)
    };

    if (data[i].country == "US" && data[i].admin1) {
      city.state = data[i].admin1;
    } else if (data[i].country == "CA" && data[i].admin1 in CA_ADMIN_CODE_TO_PROVINCE) {
      city.state = CA_ADMIN_CODE_TO_PROVINCE[data[i].admin1];
    }

    if (data[i].country) {
      city.country = COUNTRY_ISO_TO_NAME[data[i].country];
    }

    city.nameNormalized = normalizeCityForPrefixMatch(city, city.name);

    var aliases = data[i].alt_name.trim();
    if (aliases) {
      city.aliases = aliases.split(",");
      city.aliasesNormalized = city.aliases.map(function(alias) {
        return normalizeCityForPrefixMatch(city, alias);
      });
    }

    if (city.population > maxPopulation) {
      maxPopulation = city.population;
    }

    cities.push(city);

    var firstChar = city.nameNormalized.codePointAt(0);
    if (firstChar in citiesByFirstChar) {
      if (citiesByFirstChar[firstChar].indexOf(city) == -1) {
        citiesByFirstChar[firstChar].push(city);
      }
    } else {
      citiesByFirstChar[firstChar] = [];
      citiesByFirstChar[firstChar].push(city);
    }

    if (city.aliasesNormalized) {
      city.aliasesNormalized.map(function(aliasNormalized) {
        var firstChar = aliasNormalized.codePointAt(0);
        if (firstChar in citiesByFirstChar) {
          if (citiesByFirstChar[firstChar].indexOf(city) == -1) {
            citiesByFirstChar[firstChar].push(city);
          }
        } else {
          citiesByFirstChar[firstChar] = [];
          citiesByFirstChar[firstChar].push(city);
        }
      });
    }
  };
}

console.log("SuggestionEngine: Memory usage before init",  process.memoryUsage());
init();
//global.gc();
console.log("SuggestionEngine: Memory usage after init",  process.memoryUsage());

function match(city, query, latitude, longitude) {
  // Scoring criteria:
  // - The main criteria is prefix matching on the name (and aliases) of the city, 
  //   with 0.6 of the weight. If there is prefix mismatch in terms of naming, we 
  //   immediately return a score of 0 rather than computing the rest.
  // - Compute a non-relative score that is consistent over different calls, instead 
  //   of a relative score amongst cities that match
  // - Distance is allocated a weight of 0.3, but it cuts off once the query 
  //   lat/long is just two time zones away from the city
  // - Making the Inference that a user is more likely to search for more populous 
  //   cities, we give population a weight of 0.2
  // - The weights add up to more than 1, but maxes out at 1
  // - In practice, it is probably better to look into scoring suggestions based on
  //   search volume/history rather than on just city data

  var name = city.name;
  var prefix = prefixMatch(query, city.nameNormalized);

  if (prefix == -1 && city.aliases) {
    for (var i = 0; i < city.aliases.length; i++) {
      name = city.aliases[i];
      prefix = prefixMatch(query, city.aliasesNormalized[i]);
      if (prefix != -1) {
        break;
      }
    }
  }

  if (prefix == -1) {
    // Prefix mismatch for name and aliases
    return { score: 0 };
  }

  var score = 0;

  if (prefix == name.length) {
    score = 0.6; // Perfect match
  } else {
    score = 0.3 + ((0.3 * prefix) / name.length); // Partial match, score based on how many chars matched
  }

  if (!isNaN(latitude) && !isNaN(longitude)) {
    var dist = Math.sqrt(Math.pow(city.latitude - latitude, 2) + Math.pow(city.longitude - longitude, 2))
    score += Math.max(0, SCORING_DIST_THRESHOLD - dist) / SCORING_DIST_THRESHOLD * 0.3;
  }

  score += (city.population/maxPopulation) * 0.2;

  score = Math.min(1.0, score);

  return {
    score: score,
    name: makeDisplayName(city, name),
    latitude: city.latitude,
    longitude: city.longitude
  };
}


function prefixMatch(query, target) {
  var i = 0;
  for (i = 0; i < query.length; i++) {
    if (i >= target.length) {
      return -1; // Prefix mismatch
    }
    if (target.codePointAt(i) != query.codePointAt(i)) {
      return -1; // Prefix mismatch
    }
  }
  return i;
}

function normalizeQuery(query) {
  // Replace all delimiter characters (space, comma) with a single space.
  // Lowercase everything.
  var terms = query.trim().toLowerCase().split(/[\s,]+/);
  return terms.join(" ")
}

function normalizeCityForPrefixMatch(city, name) {
  var displayName = makeDisplayName(city, name);
  return normalizeQuery(displayName);
}

function makeDisplayName(city, name) {
  var display = name;
  if (city.state) {
    display += ", " + city.state;
  }
  if (city.country) {
    display += ", " + city.country;
  }
  return display;
}



var SuggestionEngine = function() { 
};

SuggestionEngine.prototype.get = function(query, latitude, longitude) {
  if (typeof query != 'string' || query.trim().length < MIN_QUERY_LENGTH) {
    return [];
  }

  query = normalizeQuery(query);
  latitude = parseFloat(latitude);
  longitude = parseFloat(longitude);

  // Look only at the cities with the same first character
  var firstChar = query.codePointAt(0);
  if (!(firstChar in citiesByFirstChar)) {
    return [];
  }
  var matchedCities = citiesByFirstChar[firstChar];

  var suggestions = new Heap(function(s1, s2) {
    return s1.score - s2.score; // Min heap of suggestions
  });

  for (var i = 0; i < matchedCities.length; i++) {
    var suggestion = match(matchedCities[i], query, latitude, longitude);
    if (suggestion.score == 0) {
      continue; // Skip all scores of 0
    }

    // Maintain a heap of top scores, and add into the list if necessary
    if (suggestions.size() < SUGGESTIONS_LIMIT) {
      suggestions.push(suggestion);

    } else if (suggestion.score > suggestions.peek().score) {
      suggestions.pop();
      suggestions.push(suggestion);
    }
  }

  return suggestions.toArray().sort(function(s1, s2) {
    return s2.score - s1.score; // Sort descending by score
  });
};

module.exports = new SuggestionEngine();
