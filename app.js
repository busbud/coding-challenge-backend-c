var http = require('http');
var url = require('url');
var geolib = require('geolib'); 
var MongoClient = require('mongodb').MongoClient;

var port = process.env.PORT || 2345;
var env = process.env.NODE_ENV || "development";

var myLat = 45.468462;
var myLong = -73.620843;

try {
	module.exports = http.createServer(function(req, resp) {
		var arrResults = [];
		if (req.url.indexOf('/suggestions') === 0) {
			// decode querystring
			var parts = url.parse(req.url, true);
			// ensure we have search text in the q argument
			if (parts.query['q'] == undefined || parts.query['q'].trim() == "") {
				// log('invalid q passed in');
				resp.writeHead(404, {'Content-Type':'application/json'});
				resp.end(JSON.stringify({"suggestions":arrResults}) + '\n');
			} else {
				var q = parts.query['q'];
				// check if we have any results
				MongoClient.connect("mongodb://localhost:27017/busbud", function(err, db) {
					if(err) { 
						console.error('connection error: ' + err);
						process.exit(1);
					}
				    var collection = db.collection('cities');
				    var popLimit = 5000;
				    var regex = new RegExp(q, 'i')
				    collection.find({
				    	alt_name:regex, // search with alt_name so can handle variations, eg. Montreal vs. Montréal
				    	population:{$gt: popLimit}
				    }
				    ,{ name:1, alt_name:1, country:1, admin1:1, lat:1, long:1, _id:0 }	
				    ).sort({name:1}).toArray(function(err, results) {
				    	if (err != null) {
				    		console.error('find error: ' + err);
				    		process.exit(2);
				    	} else {
				            if (results == undefined || results.length == 0) {
				                log("0 results returned for '" + q + "'");
								resp.writeHead(404, {'Content-Type':'application/json'});
				            	resp.end(JSON.stringify({"suggestions":arrResults}) + '\n');
				            } else {
				                log(results.length + " results returned for '" + q + "'");
				                results.forEach(function(entry) {
				                	var fullcity = getFullCity(entry);
				                	var score = getScore(q, entry);
							var distance = getDistance(parts, entry['lat'], entry['long']);
				                	arrItem={"name":fullcity,
				                			"alt_name":entry['alt_name'],
				                			"latitude":entry['lat'],
				                			"longitude":entry['long'],
									"distance":distance,
				                			"score":score
				                		};
				                	arrResults.push(arrItem);
				                });
				                // TODO: sort results by score
				                var sortedResults = arrResults.sort(compare);
				                // present sorted results
								resp.writeHead(200, {'Content-Type':'application/json'});
								resp.end(JSON.stringify({"suggestions":sortedResults}) + '\n');
				                db.close(); // will exit the process
				            }
				    	}
				    });
				});
			}
		} else {
			resp.writeHead(302, {
				'Location': '/suggestions?q=&latitude=&longitude='
				});
			resp.end();
		}
	}).listen(port, '0.0.0.0');
} catch (e) {
	log("oops");
}

function isNumeric(obj) {
    obj = typeof(obj) === "string" ? obj.replace(",", ".") : obj;
    return !isNaN(parseFloat(obj)) && isFinite(obj) && Object.prototype.toString.call(obj).toLowerCase() !== "[object array]";
};

function log(str) {
	if (env == 'development') console.log(str);
}

function compare(a,b) {
	if (a.score < b.score)
		return 1;
	if (a.score > b.score)
		return -1;
	if (a.score = b.score) {
		if (a.distance > b.distance)
			return 1;
		if (a.distance < b.distance)
			return -1;
	}
	return 0;
}

var provinces = {
	1:'AB',
	2:'BC',
	3:'MB',
	4:'NB',
	5:'NL',
	6:'',
	7:'NS',
	8:'ON',
	9:'PE',
	10:'QC',
	11:'SK',
	12:'YT',
	13:'NT',
	14:'NU'
}

var testCities = {
	'montreal': {'latitude':'-74.00597', 'longitude':'-73.64918'},
	'new york': {'latitude':'40.71427', 'longitude':'-74.00597'},
	'los angeles': {'latitude':'34.05223', 'longitude':'-118.24368'},
	'vancouver': {'latitude':'49.24966', 'longitude':'-123.11934'},
	'miami': {'latitude':'25.77427', 'longitude':'-80.19366'}
}

function getFullCity(entry) {
	if (entry['country'] == 'US') {
		return entry['name'] + ", " + entry['admin1'] + ", USA";
	} else {
		return entry['name'] + ", " + provinces[entry['admin1']] + ", Canada";
	}
}

function getDistance(parts, lat2, long2) {
	// convenience arguments for testing
	var city = parts.query['city'].toLowerCase();
	if (city != undefined && city.trim() != '' && testCities[city]) {
		myLat = testCities[city]['latitude'];
		myLong = testCities[city]['longitude'];
	} else {
		myLat = parts.query['latitude'];
		myLong = parts.query['longitude'];
	}
	// only compute distance if we were passed in valid numeric arguments
	if (myLat == undefined || myLat.trim() == '' || 
		myLong == undefined || myLong.trim() == '' ||
		isNumeric(myLat) == false || isNumeric(myLong) == false) {
		return NaN;
	} else {
		var x = geolib.getDistance(
		    {latitude: myLat, longitude: myLong}, 
		    {latitude: lat2, longitude: long2}
		);
		return x/1000;
	}
}

function getScore(qSearchString, entry) {
	var score = 0;
	var regex = new RegExp(qSearchString, 'gi')
	// RULE: add .5 if the searched city is an exact match for the name
	if (qSearchString.trim().toLowerCase() == entry['name'].trim().toLowerCase()) {
		score += .5;
	// TODO: add .2 or .3 if the name is a partial match, i.e. York from New York
	} else if (Array.isArray(entry['name'].match(regex))) {
		score += .3;
	// RULE: add .1 if the searched city is only included in the alt_name field
	} else {
		score += .1;
	}
	return score;
}

log('Server running at http://127.0.0.1:' + port + '/suggestions');
