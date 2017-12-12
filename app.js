const express = require('express'),
    app = express();
const port = process.env.PORT || 5000;

//Define global var cities
global.cities = [];

//Init cities from local storage then load from remote
const cityManager = require('./api/modules/cityManagerModule');
cityManager.initCities(); //from local storage
cityManager.loadCities(); //from remote

//Start the server
app.listen(port);


//register the routes
const configurationRoutes = require('./api/routes/configurationRoutes');
configurationRoutes(app);

const suggestionsRoutes = require('./api/routes/suggestionsRoutes');
suggestionsRoutes(app);

//Send 404 error on all undefined routes
app.use(function(req, res){
    res.sendStatus(404);
});

console.log('Server running on port %d', port);

//In order to use it in the tests
module.exports = app;