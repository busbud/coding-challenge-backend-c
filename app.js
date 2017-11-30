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
		if (!req.url.startsWith(basePath) || req.url.length == basePath.length) {

			res.writeHead(400, { 'Content-Type': 'text/plain; charset=utf-8' });
			res.end("Service only responds on non empty query beginning with " + basePath);

		} else {
			var urlChunks = req.url.split("&"); // split creates up to 3 new strings, can this be avoided ? (with Buffer for example)
			
			var query = decodeURIComponent(urlChunks[0].substring(basePath.length)).toLowerCase();
			var result = search(query, () => parseLatLon(...urlChunks)); // we pass an arrow function in order to avoid lat/lon parsing if it is not needed (cleaneness can be improved)

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

var parseGeoParam = (str, paramPrefix) => str.startsWith(paramPrefix) && parseFloat(str.substring(paramPrefix.length));

var parseLatLon = function (query, latStr, lonStr) {
	var lat = latStr && parseGeoParam(latStr, latParam);
	var lon = lonStr && parseGeoParam(lonStr, lonParam);
	if (lat && lon && !isNaN(lat) && !isNaN(lon))
		return [lat, lon];
}