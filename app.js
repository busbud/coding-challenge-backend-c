const express = require('express'),
    app = express();
const port = process.env.PORT || 5000;

//Load cities
const cityManager = require('./api/modules/cityManagerModule');
cityManager.loadCities('data/cities_canada-usa.tsv', "\t");

//Start the server
app.listen(port);


//register the routes
const routes = require('./api/routes/suggestionsRoutes');
routes(app);

console.log('Server running on port %d', port);

//In order to use it in the tests
module.exports = app;