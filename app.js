var http = require('http');
var port = process.env.PORT || 2345;

// Thom
var suggestionsController = require('./controllers/suggestions');
const dataFileName = process.env.CITIES_FILE_NAME || 'cities_canada-usa';
var parser = require('./services/cities-parser');

////// Database setup & connextion
let mongoose = require('mongoose');
mongoose.Promise = global.Promise;
let uristring = process.env.MONGOLAB_URI || process.env.MONGOHQ_URL || 'mongodb://localhost/busbug-challenge';

mongoose.connect(uristring, { useMongoClient: true }, (err, res) => {
    if (err) {
        console.log('ERROR connecting to: ' + uristring + '. ' + err);
    } else {
        // When connected, check if cities collection exists
        mongoose.connection.db.listCollections({ name: 'cities' })
        .next((err, citiesCollectionInfo) => {
            // If cities collection does not exist
            if (!citiesCollectionInfo) {
                // Generate data at server start
                let data;
                parser(dataFileName).then(cities => {
                    // Import data in DB
                    let cityModel = mongoose.model('City', citySchema);
                    cityModel.insertMany(cities);
                });
            }
        });

        console.log('Succeeded connected to: ' + uristring);
    }
});



var citySchema = new mongoose.Schema({
    ident: {
        type: String,
        trim: true
    },
    name: {
        type: String,
        trim: true
    },
    coordinates: [Number]
});


module.exports = http.createServer(function (req, res) {
    res.writeHead(404, { 'Content-Type': 'text/plain' });

    if (req.url.indexOf('/suggestions') === 0) {
        return suggestionsController(req, res, data);
    } else {
        res.end();
    }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);