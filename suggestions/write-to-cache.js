var rs = require('rocket-store');
rs.options({
  data_storage_area: __dirname + "/cache-db",
  data_format: rs._FORMAT_JSON
});

/*
* Adds key to data store if not exists, and updates the value of the count for the key
* 
* @async
* @param {object} suggestion object
* @returns {void}
*/
function updateCountsToFile(suggestion) {
  var newCount;
  var newTotal;
  rs.get("suggestions", suggestion.name.trim().toLowerCase())
  .then(function (result) {
    if (result && result.result && result.result[0] && result.result[0].count) {
      newCount = result.result[0].count + 1;
    }
    else {
      newCount = 1;
    }
    return rs.post("suggestions", suggestion.name.trim().toLowerCase(), {count: newCount});
  })
  .then(function () {
    return rs.get("totals", "matchCount");
  })
  .then(function (result) {
    if (result && result.result && result.result[0] && result.result[0].count) {
      newTotal = result.result[0].count + 1;
    }
    else {
      newTotal = 1;
    }
    rs.post("totals", "matchCount", {count: newTotal});
  })
  .catch(function (err) {
    console.error(err);
  })
}

/*
* Stores suggestion objects to filesystem cache
* 
* @async
* @param {array} list of suggestion objects
* @returns {void}
*/
module.exports = function (suggestions) {
  for (var i = 0; i < suggestions.length; i++) {
    updateCountsToFile(suggestions[i]);
  }
}