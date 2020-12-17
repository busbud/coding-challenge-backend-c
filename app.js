// Node.js Embedded Libraries
const http = require('http');
const port = process.env.PORT || 2345;
const url = require('url');

// Module
var functions = require('./functions');

module.exports = http.createServer(function (req, res) {
  if (req.url.indexOf('/suggestions') === 0) {
    res.writeHead(200, {'Content-Type': 'application/json'});
	
	const getRequestParams = url.parse(req.url, true).query;
	//console.log(getRequestParams); // debug
	
    res.end(JSON.stringify({
      suggestions: functions.searchInArray(getRequestParams.q, getRequestParams.latitude, getRequestParams.longitude)
    }, null, 3));
  } 
  else {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);




// Challenge solved by Tarik Seyceri - tarik@seyceri.info - Istanbul, Turkey