var http = require('http');
var url = require('url');
var getCityDataAndScoreFromGeoDataModule = require('./getCityDataAndScoreFromGeoData.js');
var port = process.env.PORT || 2345;

const requestHandler = (request, response) => {

	var req = url.parse(request.url, true);

	const urlSearch = new URLSearchParams(req.search);
	var errorsDuringValidation = validateClientRequestAndGetErrors(req.pathname, urlSearch);
	if(errorsDuringValidation == null) {
		var region = urlSearch.get('q');

		// During validation we check either both latitude and longitude are present or neither.
		var longitude = null;
		var latitude = null;
		if(urlSearch.has('latitude')  && urlSearch.has('longitude')) {
			latitude = parseFloat(urlSearch.get('latitude'));
			longitude = parseFloat(urlSearch.get('longitude'));
		}

		getCityDataAndScoreFromGeoDataModule.getCityDataWithScore(region, latitude, longitude, function(err, resultJson, responseStatusCode) {
			if(!err) {
				console.log('Status code ');
				response.statusCode = responseStatusCode;
				response.write(resultJson);
				response.end();
				return;
			}
			else {
				console.log('Error ');
				response.statusCode = responseStatusCode;
				response.end(err.message);
				return;
			}
		});
	}

	else {
		response.statusCode = 400;
		console.log("Bad request: " + req.toString() + " error: " + errorsDuringValidation);
		response.write(errorsDuringValidation);
		response.end();
		return;
	}

}

function validateClientRequestAndGetErrors(requestPath, urlSearch) {
	if(requestPath != '/suggestions') {
		return 'Request path: ' + requestPath + ' invalid.';
	}

	if(!urlSearch.has('q')) {
		return 'Request missing parameter q.';
	}

	if(!urlSearch.get('q') && urlSearch.get('q').length == 0) {
		return 'Request contains invalid value for q.';
	}

	if(!urlSearch.has('latitude')  && urlSearch.has('longitude')) {
		return 'Request contains parameter longitude but is missing latitude.';
	}

	if(!urlSearch.has('longitude')  && urlSearch.has('latitude')) {
		return 'Request contains parameter latitude but is missing longitude.';
	}

	if(urlSearch.has('latitude')  && urlSearch.has('longitude')) {
		lat = parseFloat(urlSearch.get('latitude'));
		longitude = parseFloat(urlSearch.get('longitude'));
		if(isNaN(lat) || isNaN(longitude)) {
			return 'Request contains invalid values for latitude/longitude.';
		}
	}

	return null;
}

http.createServer(requestHandler).listen(port, '127.0.0.1');
