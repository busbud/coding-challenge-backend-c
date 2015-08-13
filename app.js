var http = require('http');
var port = process.env.PORT || 2345;
var url = require('url');
var city = require('./controllers/cities.js');

module.exports = http.createServer(function (req, res) {

    if (req.url.indexOf('/suggestions') === 0) {
        var query = url.parse(req.url,true).query;
        if(typeof query.q !== "undefined"){
            city.getCities(query.q, query.latitude, query.longitude, function(suggested_cities){
                var statusCode = (suggested_cities.length > 0) ? 200 : 404;
                res.writeHead(statusCode, {'Content-Type': 'application/json'});
                res.end(JSON.stringify({
                    suggestions: suggested_cities
                }));
            });
        } else {
            res.end();
        }
    } else {
        res.end();
    }

}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
