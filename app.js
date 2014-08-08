var http = require('http');
var url = require('url');
var port = process.env.PORT || 2345;

var sqlite3 = require('sqlite3').verbose();
var db = new sqlite3.Database(':memory:');

db.exec("CREATE TABLE cities (id int(11) NOT NULL, name varchar(200) NOT NULL, alternateNames varchar(5000) DEFAULT NULL, country varchar(2) DEFAULT NULL, lat double NOT NULL, lon double NOT NULL, PRIMARY KEY (id));");

function populateDB() {
	var done = false;
	// use streams to pipeline data from the file to memory
	var fs = require("fs");
	var es = require("event-stream");
 	var insert = db.prepare("INSERT INTO `cities` (`id`,`name`,`alternateNames`,`country`,`lat`,`lon`) values(?,?,?,?,?,?);");
	// Read File
	fs.createReadStream("data/cities_canada-usa.tsv")
		// split records
	    .pipe(es.split("\n"))
	    // parse record fields into an array
	    .pipe(es.mapSync(function(data) {
        	return data.split("\t");
    	}))
    	// insert records into DB
    	.pipe(es.through(function write(data) {
			this.emit('data', data);
			//console.log(data[1]);
			insert.run(data[0],data[1], data[3],data[8],data[4],data[5]);
		}));
	// TODO: should block here to prevent access before the DB is populated
}

function rankByConfidence(a, b) {
	// descending order
	if (a.score > b.score) {
		return -1;
	}
	if (a.score < b.score) {
		return 1;
	}
	return 0;
}

function calculateConfidenceMetrics(data) {
	//this [substrings]

	// length of matched substrings / total length of name
	// TODO: take the number of matched strings into account later for better confidence heuristic
	var confidenceMetrics = {matchedSubstrings: 0.0, matchedLength: 0.0, distance: 0.0};
	
	var totalLength = 0.0;
	for (var i = 0; i < this.length; i++) {
		if (data.name.toLowerCase().indexOf(this[i].toLowerCase()) != -1) {
			confidenceMetrics.matchedSubstrings++;
			confidenceMetrics.matchedLength += this[i].length;
		}
	};
	totalLength += data.name.length;

	// it is possible (but unlikely) for matches to overlap with this calculation
	data.confidence = totalLength > 0 ? confidenceMetrics.matchedLength / totalLength : 0;
	if (data.confidence > 1) {
		data.confidence = 1;
	}

	return data;
}

function prepareResponseObject(data) {
	return { 
		name: data.name + ", " + data.country,
		latitude: data.lat,
		longitude: data.lon,
		score: data.confidence
	};
}

function distanceFromCityConfidence(data) {
	// this = location {lat: Number, lon: Number}

	// distance calculation thanks to: http://www.mapanet.eu/EN/resources/Script-Distance.htm
	var lat1 = Math.PI / 180 * this.lat;
	var lon1 = Math.PI / 180 * this.lon;

	var lat2 = Math.PI / 180 * data.lat;
	var lon2 = Math.PI / 180 * data.lon;

	var distance = 6378.137 * Math.acos(Math.cos(lat1) * Math.cos(lat2) * Math.cos(lon2 - lon1) + Math.sin(lat1) * Math.sin(lat2));

	// assume perfect distance match if less than 10km away.
	if (distance > 10) {
		data.confidence *= 0.9;
		data.confidence += 0.1 * (1 / distance);
	}
	return data;
}

function suggestedCitiesResponse(substrings, res, params) {

	// build matching query (consider alternateNames for completeness)
	var query = "SELECT DISTINCT id, name, alternateNames, country, lat, lon FROM cities WHERE ";
	query += "name LIKE '%";
	query += substrings.join("%' OR name LIKE '%");
	query += "%' or alternateNames LIKE '%";
	query += substrings.join("%' OR alternateNames LIKE '%");
	query += "%';";

	db.all(query, function(err, data) {
		if (data === null || data.length === 0) {
			emptyResponse(res);
		} else {
			// calculate confidence in each result
			var computed = data.map(calculateConfidenceMetrics, substrings);

			// factor in the lat/lon if supplied
			if (params.hasOwnProperty('latitude') && params.hasOwnProperty('longitude') && !isNaN(params.latitude) && !isNaN(params.longitude)) {
				// use distance as a confidence metric
				var location = {
					lat: parseFloat(params.latitude, 10),
					lon: parseFloat(params.longitude, 10)
				};

				// use the inverse of the distance as a 10% confidence metric
				computed = computed.map(distanceFromCityConfidence, location);
			}

			// clean up and send response
			computed = computed.map(prepareResponseObject);
			computed = computed.sort(rankByConfidence);
			suggestionsResponse(res, computed);
		}
	});
}

function badRequestResponse(res, message) {
	res.writeHead(400, {'Content-Type': 'text/plain'});
	res.end(message);
}

function emptyResponse(res)  {
	res.writeHead(404, {'Content-Type': 'text/plain'});
	res.end(JSON.stringify({
		suggestions: []
	}));
}

function suggestionsResponse(res, objects) {
	res.writeHead(200, {'Content-Type': 'application/json'});
	res.end(JSON.stringify({
		suggestions: objects
	}));
}

// load data into memory everytime we start the server
populateDB();

module.exports = http.createServer(function (req, res) {
 
  if (req.url.indexOf('/suggestions') === 0) {
  	// parse the query parameters
  	var params = url.parse(req.url, true).query;
  	
  	//validate that we have the required parameter - q
  	if (params == null || !params.hasOwnProperty('q') || params.q.length === 0) {
  		badRequestResponse(res, 'You must supply the query parameter q without an empty string');
  		return;
  	}

  	var substrings = params.q.split(/\s+/);

	// return the suggested cities response
  	suggestedCitiesResponse(substrings, res, params);

  } else {
  	//
    res.end();
  }
}).listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);