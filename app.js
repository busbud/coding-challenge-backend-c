var http = require('http');
var url = require('url');

import loadData, { findMatches } from './data/data-loader';

var port = process.env.PORT || 2345;

var server = http.createServer(function (req, res) {

	if (req.url.indexOf('/suggestions') === 0) {
		const { query } = url.parse(req.url, true);
		const suggestions = findMatches(query.q);

		if (suggestions.length) {
			res.writeHead(200, {'Content-Type': 'application/json'});
		} else {
			res.writeHead(404, {'Content-Type': 'text/plain'});
		}

		res.end(JSON.stringify({
			suggestions
		}));
	} else {
		res.end();
	}
});

loadData().then(() => {
  console.log('Finished loading data.');
  server.listen(port, '127.0.0.1');
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
}).catch((err) => {
  console.error(err);
  process.exit(1);
});

module.exports = server;