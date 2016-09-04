var Q        = require('q');
var fs       = require('fs');
var csv      = require("csv-stream");
var mongoose = require('mongoose');
var config   = require('./config/config');
var City     = require('./models/city');


var launchCitiesInsertionScript = function() {

    console.log('Start the parser script to insert all data in the .tsv file');

    // All of these arguments are optional.
    var options = {
        delimiter : '\t',
        endLine : '\n'
    };

    var csvStream = csv.createStream(options);

    var dataStream      = fs.createReadStream(config.dataFileLocation).pipe(csvStream);
    var numberInserted  = 0;
    var numberOfRecords = 0;


    console.log('Importation in progress …');
    dataStream.on('data', function(record) {

        numberOfRecords += 1;

        var city = new City(record);
        city.longitude  = record.long;
        city.latitude   = record.lat;
        city.coords     = [city.longitude, city.latitude];

        return city.save(function(error) {

            if(error)
                return console.log(error);

            numberInserted += 1;

            if(numberInserted == numberOfRecords) {
                dataStream.emit('all-data-inserted');
            }
        });

    });

    dataStream.on('all-data-inserted', function() {
        console.log('Import finished');
        console.log('Number of cities imported :', numberInserted);
        return process.kill(0);
    })
};


(function() {

    // Connect to the mongo database
    // Once connected , emit the db:connected event
    mongoose.connect(config.mongoUrl, function(error) {

        if(error) {
            app.emit('db:error', error);
            return process.kill(0);
        }


        // Remove all cities to avoid doublons
        // The launch the script to insert
        // all cities into the database
        City.remove({}, launchCitiesInsertionScript);

    });


}).call(this);
