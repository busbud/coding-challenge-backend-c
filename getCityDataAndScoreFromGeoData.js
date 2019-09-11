var request = require('request');

//http://api.geonames.org/searchJSON?username=ksuhiyp&continentCode=NA&name_startsWith=Toronto&cities=cities5000&fields=name,countryCode,adminCodes1,lng,lat&maxRows=100
// API reference http://www.geonames.org/export/geonames-search.html
exports.getCityDataWithScore = function(regionPrefix, latitude, longitude, callbackToReturnResult) {
	getCityDataFromGeoNames(regionPrefix, function(err, data) {
		if(!err) {
			if(data && ("geonames" in data)) {
				var resultJsonObject = getScoresForCityData(data, latitude, longitude, regionPrefix);
				var responseStatusCode;
				if(resultJsonObject.suggestions.length > 0) {
                                        responseStatusCode = 200;
                                }
                                else {
                                        responseStatusCode = 404;
                                }
				console.log("Suggestions length " + resultJsonObject.suggestions.length + ' response status code ' + responseStatusCode);
				callbackToReturnResult(null, JSON.stringify(resultJsonObject), responseStatusCode);
			}
			else {
				callbackToReturnResult(new Error("Unexpected error while fetching data"), null, 500);
			}
		}
		else {
			callbackToReturnResult(err, null, 500);
		}
	});
}

function getCityDataFromGeoNames(regionPrefix, callback) {
	var reqUri = 'http://api.geonames.org/searchJSON?';
	const userName = 'soyboyxvx702';
	reqUri = reqUri + 'username=' + userName;
	reqUri = reqUri + '&country=US';
	reqUri = reqUri + '&country=CA';
	reqUri = reqUri + '&cities=cities5000';
	reqUri = reqUri + '&fields=name,countryCode,adminCodes1,lng,lat,population';
	reqUri = reqUri + '&name_startsWith=' + regionPrefix;
	reqUri = reqUri + '&maxRows=100';
	var options = {
		url: reqUri,
		headers: {
			'X-Busbud-Token' : 'PARTNER_AHm3M6clSAOoyJg4KyCg7w'
		}
	};

	console.log("Making the following HTTP GET request to geoname: " + reqUri);

	request(reqUri, function(error, response, body) {
		if(!error && response.statusCode == 200) {
			console.log('Geoname response for region: ' + regionPrefix + ' is: ' + body);
			var resultJsonObj = JSON.parse(body);
			return callback(null, resultJsonObj);
		}
		else {
			if(!error) {
				return callback(new Error("Unexpected error while fetching data: " + body + " ,response status code " + response.statusCode), null);
			}
			return callback(error, null);
		}
	});
}

// We will use Euclidean distance from user's location or population as a metric to determine scores, depending on whether user has supplied us with their coorinates
function getScoresForCityData(jsonObject, latitude, longitude, regionPrefix) {
	var maxScore = Number.MIN_VALUE;
	var minScore = 0;

	var returnJsonObj = {
		suggestions: []
	};

	// Flag to indicate if we are using proximity to user's location to determine the score
	var areUserCoordinatesSpecified = true;
	if(latitude == null && longitude == null) {
		areUserCoordinatesSpecified = false;
	}

	for(var i = 0; i < jsonObject.geonames.length; i++) {
		// Validate and return object if all the concerned fields in the json seem valid
		var obj = validateAndGetCityDataFromJsonFields(jsonObject.geonames[i], !areUserCoordinatesSpecified, regionPrefix);

		if(obj != null) {
			if(areUserCoordinatesSpecified) {
				// Score is tentatively assigned to Euclidean distance from user's current location
				obj.score = Math.pow(Math.pow(obj.longitude - longitude, 2) + Math.pow(obj.latitude - latitude, 2), 0.5);
			}
			else {
			// Score is tentatively assigned to the regionPrefix's log(population)
				obj.score = Math.log10(jsonObject.geonames[i].population);
			}
			maxScore = Math.max(maxScore, obj.score);
			returnJsonObj.suggestions.push(obj);
		}
		else {
			console.log('Encountered invalid geoname entry for region prefix ' + regionPrefix + '.');
		}
	}

	// To avoid cases where maxScore = minScore. It can happen when we have only 1 element in the array with min score
	maxScore += 1;

	for(var i = 0; i < returnJsonObj.suggestions.length; i++) {
		if(areUserCoordinatesSpecified) {
			// Substrcting current score from 1 to normalize it between 0 & 1 and give higher values to scores with lower Euclidean distance
			returnJsonObj.suggestions[i].score =  (1.0 - (returnJsonObj.suggestions[i].score - minScore) / (maxScore - minScore)).toFixed(2);
		}
		else {
			// Normalize population score between 0 and 1
			returnJsonObj.suggestions[i].score =  ((returnJsonObj.suggestions[i].score - minScore) / (maxScore - minScore)).toFixed(2);
		}
	}

	// Sort the cities in descending order of scores
	returnJsonObj.suggestions.sort(function(a, b) {return b.score - a.score} );

	return returnJsonObj;
}

function validateAndGetCityDataFromJsonFields(jsonFields, checkPopulation, regionPrefix) {
	// Validating all required fields are present in the json before returning data object
	if(
		("toponymName" in jsonFields) && (jsonFields.toponymName) && (jsonFields.toponymName.length > 0)
		&& ("name" in jsonFields) && (jsonFields.name) && (jsonFields.name.length > 0)
		&& jsonFields.name.includes(regionPrefix) || jsonFields.toponymName.includes(regionPrefix)
		&& ("adminCodes1" in jsonFields) && ("ISO3166_2" in jsonFields.adminCodes1) && (jsonFields.adminCodes1.ISO3166_2) && (jsonFields.adminCodes1.ISO3166_2.length > 0)
		&& ("countryCode" in jsonFields) && (jsonFields.countryCode) && (jsonFields.countryCode.length > 0)
		&& ("lat" in jsonFields) && !isNaN(jsonFields.lat)
		&& ("lng" in jsonFields) && !isNaN(jsonFields.lng)
		&& (!checkPopulation || (checkPopulation && ("population" in jsonFields) && !isNaN(jsonFields.population) && jsonFields.population > 0))
	) {
		var obj = new Object();
		obj.name = jsonFields.name + ", " + jsonFields.adminCodes1.ISO3166_2 + ", " + jsonFields.countryCode;
		obj.latitude = jsonFields.lat;
		obj.longitude = jsonFields.lng;

		return obj;
	}
	return null;
}

