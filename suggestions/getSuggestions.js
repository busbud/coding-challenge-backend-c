var rl = require('readline');
var fs = require('fs');
var allScores = require('./score-imports.js');
var config = require('./config.json');
var writeToCache = require('./write-to-cache.js');

/*
* Reads the tsv file row by row to be scored against the query
* 
* @async
* @param {URL.searchParams} the query parameters of the http request
* @param {Function} callback function which returns an array of 0 to 3 suggestions
* @returns {void}
*/
function readTSV (query, callback) {
  var topResults = [];
  var countTotalRows = 0;
  var countParsedRows = 0;
  var done = false;
  /*
  * Checks that all tsv rows have been evaluated, then calls the callback of `readTSV`
  * 
  * @returns {void}
  */
  function checkComplete() {
    if (countTotalRows > 0 && countTotalRows === countParsedRows) {
      writeToCache(topResults);
      callback(topResults);
    }
  }

  /*
  * Evaluates one row against all scoring functions and adds best-ranking suggestions to results array
  * 
  * @async
  * @param {Array} array of strings representing each column of the tsv file
  * @returns {void}
  */
  function scoreRow(row) {
    var promises = [];
    var weights = [];
    for (var i = 0; i < allScores.length; i++) {
      promises.push(allScores[i].f(query, row));
      weights.push(allScores[i].weight)
    }
    Promise.all(promises)
    .then(function (scores) {
      var cumulativeScore = 0;
      var topScore = 0;
      var rowScore = 0;
      for (var j = 0; j < scores.length; j++) {
        if (typeof scores[j] === 'number') {
          cumulativeScore += (scores[j] * weights[j]);
          topScore += weights[j];
        }
      }
      rowScore = cumulativeScore / topScore;

      /*
      * Builds the a suggestion object and inserts it into the correct place in the results array
      * 
      * @param {number} the number representing the index the suggestion should occupy in the results array
      * @returns {void}
      */
      function addToResults(place) {
        var city = row[config.data_columns.ascii];
        var state = row[config.data_columns.admin1];
        var country = config.country_names[row[config.data_columns.country]];
        if (row[config.data_columns.country] === "CA") {
          state = config.canada_provinces[row[config.data_columns.admin1]];
        }
        var suggestion = {
          "name": city + ", " + state + ", " + country,
          "latitude": row[config.data_columns.lat].toString(),
          "longitude": row[config.data_columns.long].toString(),
          "score": rowScore
        }
        topResults.splice(place, 0, suggestion)
        if (topResults.length > 3) {
          topResults.pop();
        }
      }
      switch(true) {
        case rowScore <= 0:
        break;
        case topResults.length < 3:
        addToResults(0);
        break;
        case rowScore > topResults[0].score:
        addToResults(0);
        break;
        case rowScore > topResults[1].score:
        addToResults(1);
        break;
        case rowScore > topResults[2].score:
        addToResults(2);
        break;
      }
      countParsedRows++;
      if (done) {
        checkComplete();
      }
    })
    .catch(function (err) {
      console.error(err);
      countParsedRows++;
    })
  }
  // how do I iterate through a tsv without writing the whole thing into memory?
  // https://stackoverflow.com/a/64294963
  rl.createInterface({input: fs.createReadStream(__dirname + '/../data/cities_canada-usa.tsv')})
  .on('line', function(input) {
    var row = input.split(/\t/gm);
    // Check that the row in the database meets the minimum population requirement
    if (row[config.data_columns.population] && +row[config.data_columns.population] > config.limit.population.min) {
      countTotalRows++;
      scoreRow(row);
    }
  })
  .on('close', function () {
    done = true;
    checkComplete();
  })
}

/*
* Initiates the search for suggestions and returns the result to the http server
* 
* @async
* @param {URL.searchParams} the query parameters of the http request
* @returns {Promise} resolves an object `suggestions` containing an array of 0 to 3 suggestions
*/
function getSuggestions(query) {
  return new Promise (function (resolve, reject) {
    readTSV(query, function (suggestions) {
      resolve({ suggestions });
    });
  })
}

module.exports = getSuggestions;