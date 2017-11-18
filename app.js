var http = require('http');
import loadData from './data/data-loader';
var port = process.env.PORT || 2345;

var server = http.createServer(function (req, res) {
	res.writeHead(404, {'Content-Type': 'text/plain'});

	if (req.url.indexOf('/suggestions') === 0) {
		res.end(JSON.stringify({
			suggestions: []
		}));
	} else {
		res.end();
	}
});

loadData().then(() => {
  console.log('Finished loading data.')
  server.listen(port, '127.0.0.1');
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
}).catch((err) => {
  console.error(err);
  process.exit(1);
});

module.exports = server;