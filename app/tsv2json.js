var fs = require("fs");
var es = require("event-stream");
var path = require("path");
var reduce = require("stream-reduce");

function parseToJSON() {
    var isColumnNames = true;
    var columnNames = [];
    var tabRegex = new RegExp(/\t/);

    return function parseHelper(line) {
        //runs the first time this function is called.
        //the data chunk received from the stream
        //is the column headers
        if (isColumnNames) {
            columnNames = line.split(tabRegex);
            isColumnNames = false;
            return {};
        }
        else {
            var cityInfo = line.split(tabRegex);
            var _cityInfo = {};
            //create the city info JSON object
            for (var index = 0; index < columnNames.length; index++) {
                var columnName = columnNames[index];
                _cityInfo[columnName] = cityInfo[index] == undefined || cityInfo[index] == "" ? JSON.stringify(null) : cityInfo[index];
            }
        }
        return _cityInfo;
    }
}

module.exports = function tsv2JSON(pathToFile, outputFileName) {

    return new Promise(function(resolve, reject) {
        var parsedPath = path.parse(pathToFile);
        var pathToOutputFile = path.resolve(parsedPath.dir + "/" + outputFileName + ".json");
        var readStream = fs.createReadStream(pathToFile, "utf8");
        var writeStream = fs.createWriteStream(pathToOutputFile);
        var parser = parseToJSON();

        readStream
            .pipe(es.split("\n"))
            .pipe(
                es.map(function(city, cb) {
                    cb(null, parser(city));
                }))
            .pipe(reduce(function(acc, city) {
                return acc.concat(city);
            }, []))
            .on("data", function(cities) {
                var citiesData =
                    cities.slice(1, cities.length - 1) //remove the record containing the tsv column headers and the last record containing undefined values
                    .filter(function(city) {
                        if ((city.country == "US" || city.country == "CA") && parseInt(city.population) > 5000) {
                            return city;
                        }
                    });

                var stringifiedCities = JSON.stringify(citiesData, null, 2);

                writeStream.write(stringifiedCities);

                writeStream.end(function() {
                    resolve("Done writing");
                });

                writeStream.close();
                readStream.close();

            })
    })

}
