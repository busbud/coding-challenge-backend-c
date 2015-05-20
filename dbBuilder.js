var fs          = require('fs'),
    stream      = require('stream'),
    events      = require('events'),
    util        = require('util'),
    levenshtein = require('./levenshtein');

function dbBuilder() {
    var self = this;

    self.cityData = [];
    self._dataPath = './data/cities_canada-usa.tsv';
    self._lineByLine = new stream.Transform({objectMode: true})

    self._lineByLine._transform = function(chunk, encoding, done) {
         //convert chuck to string
         var data = chunk.toString();

         if (this._lastLineData) //append the last line to current chuck
            data = this._lastLineData + data;

         var lines = data.split('\n'); //split on new lines

         //remove last line incase it's incomplete
         this._lastLineData = lines.splice(lines.length - 1, 1)[0];

         lines.forEach(this.push.bind(this));
         done();
    }

    //handle case where there's a single last line left in stream
    //flush it...
    self._lineByLine._flush = function(done) {
         if (this._lastLineData)
            this.push(this._lastLineData);

         this._lastLineData = null;
         done();
    }

    events.EventEmitter.call(self);
    return self;
};

util.inherits(dbBuilder, events.EventEmitter);

//performs a prime read of all the data in .tsv filtering by population
dbBuilder.prototype.primeRead = function() {
    var self    = this;
    var headers = [];
    var first   = true;

    fs.createReadStream(self._dataPath)
      .pipe(self._lineByLine)
      .on('readable', function () {
        var line;
        var city;

        while (line = self._lineByLine.read()) {
            if (first) { //first line will be the headers
                headers = line.split('\t');
                first = false;
            } else {
                city = self._createCityRecord(headers, line);

                //only add city to list if population is over 5000
                if (city.population >= 5000)
                    self.cityData.push(city);
            }
        }
      })
      .on('end', function() {
          console.log('Prime Read Complete! %d records read', self.cityData.length);
          self.emit('primeReadDone');
      });

    return self;
};

dbBuilder.prototype.search = function(term, lat, long) {
    var self        = this;
    var suggestions = [];
    var score       = 0;
    var lDistance   = 0;
    var cityName    = "";

    self.cityData.forEach(function(value, index, array) {

        //only calculate distance/score if term is shorter than 1.75 * length of found city
        //some filtering to stop bigger query strings from calculating against likely non matches
        if (term.length <= Math.floor(1.75 * value.name.length)) {
            lDistance = levenshtein(term.toUpperCase(), value.name.toUpperCase());

            score = 1 - (lDistance / Math.max(term.length, value.name.length));
            score = Math.round(score * 100) / 100; // round score to two decimal places

            if (score > 0.3) {
                cityName = value.name + ", " + value.stateProv + ", " + value.country;
                suggestions.push({name: cityName, latitude: value.lat, longitude: value.long, score: score});
            }
        }
    })

    //descending sort
    if (suggestions.length > 0) {
        suggestions.sort(function(first, second) {
            if (first.score > second.score)
                return -1;
            else if (first.score < second.score) {
                return 1;
            }

            return 0;
        });
    }

    return suggestions;
}

//creates a city record from a line read in from the .tsv file
dbBuilder.prototype._createCityRecord = function(headers, line) {
    var self = this;
    var city = {};
    var cityProperties = line.split('\t');

    cityProperties.forEach(function(value, index, array) {
        //build the city object using the headers as a reference to the position of the value
        switch (headers[index]) {
            case 'ascii':
                city.name = value;
                break;
            case 'lat':
                city.lat = value;
                break;
            case 'long':
                city.long = value;
                break;
            case 'population':
                city.population = value;
                break;
            case 'country':
                city.country = self._countryCodeToName(value);
                break;
            case 'admin1':
                city.stateProv = self._admin1toStateProv(value);
                break;
        }
    });

    return city;
};

//converts the country code into a name
dbBuilder.prototype._countryCodeToName = function(countryCode) {
    if (countryCode === 'CA')
        return 'Canada';

    return 'USA';
};

//converts admin1 values to state or province
dbBuilder.prototype._admin1toStateProv = function(code) {
    //mapping taken from
    //http://download.geonames.org/export/dump/admin1CodesASCII.txt
    switch (code) {
        case '01':
            return 'AB';
        case '02':
            return 'BC';
        case '03':
            return 'MB';
        case '04':
            return 'NB';
        case '05':
            return 'NL';
        case '07':
            return 'NS';
        case '08':
            return 'ON';
        case '09':
            return 'PE';
        case '10':
            return 'QC';
        case '11':
            return 'SK';
        case '12':
            return 'YT';
        case '13':
            return 'NT';
        case '14':
            return 'NU';
        default:
            return code
    }
};

module.exports = dbBuilder;