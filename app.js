var port = process.env.PORT || 2345,
    express = require('express'),
    suggestions = require('./suggestions');

var app = express();

app.get('/suggestions', suggestions);

app.all('/*', function(req, res) {
    res.send({ errCode: 400,
               errorMess: 'The correct api endpoint is GET /suggestions with the following parameters : q for partial or complete city name, latitude (optional) and longitude (optional). Ex: GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163'});
});

app.listen(port);
console.log('Server running at http://127.0.0.1:%d/suggestions', port);

module.exports = app;