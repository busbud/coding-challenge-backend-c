var http = require('http');
var search = require("./search.js");

var port = process.env.PORT || 2345;

// WARNING there is no sanity check on parameters parsing!

var basePath = "/suggestions?q=";
var latParam = "latitude=";
var lonParam = "longitude=";

// Server start
module.exports = http.createServer(function (req, res) {
	try {
		var urlChunks = req.url.split("&");
		var q = decodeURIComponent(urlChunks[0]);

		if (!q.startsWith(basePath) || q.length == basePath.length) {

			res.writeHead(400, { 'Content-Type': 'text/plain; charset=utf-8' });
			res.end("Service only responds on query beginning with " + basePath);

		} else {

			var query = q.substring(basePath.length).toLowerCase();
			var result = search(query, () => parseLatLon(urlChunks));

			if (!result) {
				res.writeHead(404, { 'Content-Type': 'text/json; charset=utf-8' });
				res.end('{"suggestions": []}');
			} else {
				res.writeHead(200, { 'Content-Type': 'text/json; charset=utf-8' });
				result
					// could .pipe to gzip (Note: this needs a check of the accept header)
					.pipe(res);
			}
		}
	} catch (e) {
		res.writeHead(500, { 'Content-Type': 'text/plain; charset=utf-8' });
		res.end("(✖╭╮✖) Unexpected error :" + e.message);
	}
}).listen(port, () => console.log('Server running at http://0.0.0.0:%d/suggestions', port));

var parseLatLon = function (urlChunks) {
	if (urlChunks.length == 3) {
		var lat, lon;
		if (urlChunks[1].startsWith(latParam)) lat = parseFloat(urlChunks[1].substring(latParam.length));
		if (urlChunks[2].startsWith(lonParam)) lon = parseFloat(urlChunks[2].substring(lonParam.length));
		if (!isNaN(lat) && !isNaN(lon))
			return [lat, lon];
	}
}