////// Database setup & connextion
let cityModel = require('./models/city');
let mongoose = require('mongoose');
var parser = require('./services/cities-parser');

mongoose.Promise = global.Promise;
let mongoUrl = process.env.MONGODB_URI || 'mongodb://localhost/busbud-challenge';

mongodb://<dbuser>:<dbpassword>@ds159662.mlab.com:59662/heroku_wnc1hgq6

exports.connect = function() {

    // Connection to DB
    mongoose.connect(mongoUrl, { useMongoClient: true }, (err, res) => {
        if (err) {
            console.log('ERROR connecting to: ' + mongoUrl + '. ' + err);
        } else {
    
            console.log('Succeeded connected to: ' + mongoUrl);
            
            // When connected, check if cities collection exists and has documents
            cityModel.find()
                .then(cities => {
                    // If no documents in cities collection
                    if(cities.length == 0) {
                        // Get cities from TSV file
                        const dataFileName = process.env.CITIES_FILE_NAME || 'cities_canada-usa';
                        parser.getData(dataFileName).then(cities => {
                            // Import data in DB
                            return cityModel.insertMany(cities);
                        })
                        .then(cities => {
                            console.log(`${cities.length} cities imported`);
                        });
                    }
                    else {
                        console.log(`${cities.length} cities existing in DB`);
                    }
                })
        }
    });

}