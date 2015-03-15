var http = require('http');
var port = process.env.PORT || 2345;
var url = require('url');
var locations = require('./data/locations.js');

module.exports = http.createServer(function (req, res) {

    //use express routes here instead?
    if (req.url.indexOf('/suggestions') === 0) {
	var queryString = url.parse(req.url, true).query;
	locations.search(queryString, function(err, suggestions){
	    if(err || suggestions.length === 0){
		console.log(err);
		res.statusCode = '404';
		res.writeHead('404', {'Content-Type': 'text/plain'});
		res.end(JSON.stringify({suggestions : []}));
	    }
	    else{
		res.writeHead('200', {"Content-Type": "application/json; charset=utf-8;"});
		res.end(JSON.stringify({suggestions : suggestions}, null, 2));
	    }
	});		  
    }
    else{
	res.end();
    }
  }).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
