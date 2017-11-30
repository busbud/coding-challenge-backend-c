'use strict';
var http = require('http');
var search = require("./search");

const port = process.env.PORT || 2345;

module.exports = http.createServer(function (req, res) {
	try {
		// One step URL parser with regex. Returns lat & lon only if both are valid
		// Here for demo purposes. A benchmark should be done to assess speed
		// According to the following page, literal syntax Regex is compiled once at parse time : https://stackoverflow.com/questions/9750338/dynamic-vs-inline-regexp-performance-in-javascript
		var urlParserRegex = /^\/suggestions\?q=([^&\s]+)(?:&latitude=([+-]?(?:[0-9]*[.])?[0-9]+)&longitude=([+-]?(?:[0-9]*[.])?[0-9]+))?/g;
		
		var matches = urlParserRegex.exec(req.url);

		if (!matches) {

			res.writeHead(400, { 'Content-Type': 'text/plain; charset=utf-8' });
			res.end("Please format your query accordingly to " + urlParserRegex);

		} else {
			var result = search(decodeURIComponent(matches[1]).toLowerCase(), () => parseFloat(matches[2]), () => parseFloat(matches[3])); // Arrow function is used so that we parse lat & lon only if we have found records

			if (!result) {
				res.writeHead(404, { 'Content-Type': 'text/json; charset=utf-8' });
				res.end('{"suggestions": []}');
			} else {
				res.writeHead(200, { 'Content-Type': 'text/json; charset=utf-8' });
				result
					// TODO could .pipe to gzip (Note: this needs a check of the accept header)
					.pipe(res);
			}
		}
	} catch (e) {
		res.writeHead(500, { 'Content-Type': 'text/plain; charset=utf-8' });
		res.end("(✖╭╮✖) Unexpected error :" + e.message);
	}
}).listen(port, () => console.log('Server running at http://0.0.0.0:%d/suggestions', port));