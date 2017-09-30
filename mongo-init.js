////// Database setup & connextion
let cityModel = require('./models/city');
let mongoose = require('mongoose');
var parser = require('./services/cities-parser');

mongoose.Promise = global.Promise;
let uristring = process.env.MONGOLAB_URI || process.env.MONGOHQ_URL || 'mongodb://localhost/busbud-challenge';

let mongoConnect = () => {

    mongoose.connect(uristring, { useMongoClient: true }, (err, res) => {
        if (err) {
            console.log('ERROR connecting to: ' + uristring + '. ' + err);
        } else {
    
            console.log('Succeeded connected to: ' + uristring);
            
            // When connected, check if cities collection exists and has documents
            cityModel.find()
                .then(cities => {
                    // If no documents in cities collection
                    if(cities.length == 0) {
                        // Get cities from TSV file
                        const dataFileName = process.env.CITIES_FILE_NAME || 'cities_canada-usa';
                        parser(dataFileName).then(cities => {
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

module.exports = mongoConnect();

