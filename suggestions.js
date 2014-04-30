var fs = require("fs"),
    es = require("event-stream"),
    _ = require("lodash"),
    func = require("./functions");

module.exports = function(req, res){
  var query = {
    cityName: req.query.q,
    latitude: parseFloat(req.query.latitude),
    longitude: parseFloat(req.query.longitude)
  };
  var allCities = [];
  var selectedCities = [];

  if (_.isEmpty(query.cityName)) {
    res.send(400, { errCode: 400,
                    errorMess: 'Please specify a city (GET parameter: q)'});
  }
  else{
    // Import this TSV file in a stream.
    var stream = fs.createReadStream("data/cities_canada-usa.tsv");
    stream.on('open', function () {
      stream.pipe(es.split("\n"))
      .pipe(es.map(function(data, cb) {
        // rawCity contains an array with all values for the current city.
        var rawCity = data.split("\t");
        // Format this array to an object with only useful datas.
        var city = func.formatRawCity(rawCity, cb);
        if (!city) {
          cb();
        }
        else{
          // allCities is an array which contains all cities from the TSV file (each one in an object).
          allCities.push(city);
        }
      }));
    });

    stream.on('error', function(err){
      console.log(err);
      res.send(500, { errCode: 500,
                      errorMess: 'Please notify the administrator.'});
    });

    stream.on('close', function(){
      // When the stream close, so when fetching data from the file is done, it's time to pick the right suggestions according to the q parameter.
      selectedCities = func.makeSelection(allCities, query);

      var httpCode = 200;
      if (_.isEmpty(selectedCities)) {
        httpCode = 404;
      }

      res.send(httpCode, {suggestions: selectedCities});
    });

  }
};