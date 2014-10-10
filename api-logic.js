var url = require('url');
var geolib = require('geolib'); 
var util = require('util'); 
var MongoClient = require('mongodb').MongoClient;

var DEBUG_MODE = process.env.DEBUG_MODE || true;

function handleRequest(req,res) {
	var parts = url.parse(req.url, true);
	log(parts.query);
	// ensure we have search text in the q argument
	if (parts.query['q'] == undefined || parts.query['q'].trim() == '') {
		res.writeHead(404, {'Content-Type':'application/json','Access-Control-Allow-Origin':'*'});
		res.end(JSON.stringify({'suggestions':arrResults}) + '\n');
	}
	var q = parts.query['q'];
	var arrResults = [];
	MongoClient.connect(process.env.MONGOLAB_URI, function(err, db) {
		if(err) { 
			console.error('Connection error to MongoDB: ' + err);
		}
	    var collection = db.collection('cities');
	    var popLimit = 5000;
	    var regex = new RegExp('^'+q, 'i')
	    collection.find({
	    	$or: [ { ascii:regex }, { name:regex } ] ,
	    	population:{$gt: popLimit}
	    }
	    ,{ name:1, alt_name:1, ascii:1, country:1, admin1:1, lat:1, long:1, _id:0 }	// filter for required fields
	    ).sort({name:1}).toArray(function(err, results) {
	    	if (err != null) {
	    		console.error('Error finding result: ' + err);
	    	} else {
	            if (results == undefined || results.length == 0) {
	                log('0 results returned for %s', q);
	                res.writeHead(404, {'Content-Type':'application/json','Access-Control-Allow-Origin':'*'});
	            	res.end(JSON.stringify({'suggestions':arrResults}) + '\n');
	            } else {
	                log('%d results returned for %s', results.length, q);
	                results.forEach(function(entry) {
		                var fullcity = getFullCity(entry);
		                var distance = getDistance(parts, entry['lat'], entry['long']);
		                var score = getScore(distance);
		                var distance = getDistance(parts, entry['lat'], entry['long']);
		                arrItem={'name':fullcity,
		                	'latitude':entry['lat'],
		                	'longitude':entry['long'],
							'distance':distance,
		                	'score':score
		                };
		                arrResults.push(arrItem);
	                });
	                // present sorted results
	                var sortedResults = arrResults.sort(compare);
					res.writeHead(200, {'Content-Type':'application/json','Access-Control-Allow-Origin':'*'});
					res.end(JSON.stringify({'suggestions':sortedResults}) + '\n');
	                db.close(); // will exit the process
	            }
	    	}
	    });
	});	
}

function isNumeric(obj) {
    obj = typeof(obj) === 'string' ? obj.replace(',', '.') : obj;
    return !isNaN(parseFloat(obj)) && isFinite(obj) && Object.prototype.toString.call(obj).toLowerCase() !== '[object array]';
};

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

function getFullCity(entry) {
	if (entry['country'] == 'US') {
		return entry['ascii'] + ', ' + entry['admin1'] + ', USA';
	} else {
		return entry['name'] + ', ' + provinces[entry['admin1']] + ', Canada';
	}
}

var testCities = {
		'montreal': {'latitude':'45.50884', 'longitude':'-73.58781'},
		'new york': {'latitude':'40.71427', 'longitude':'-74.00597'},
		'los angeles': {'latitude':'34.05223', 'longitude':'-118.24368'},
		'vancouver': {'latitude':'49.24966', 'longitude':'-123.11934'},
		'miami': {'latitude':'25.77427', 'longitude':'-80.19366'}
	}

function getDistance(parts, lat2, long2) {
	// convenience arguments for testing
	var city = parts.query['city'];
	if (city != undefined && city.trim() != '' && testCities[city.toLowerCase()]) {
		myLat = testCities[city.toLowerCase()]['latitude'];
		myLong = testCities[city.toLowerCase()]['longitude'];
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
		var distance = geolib.getDistance(
		    {latitude: myLat, longitude: myLong}, 
		    {latitude: lat2, longitude: long2}
		)/1000; // divide by 1000 to convert meters to kilometers
		return Math.floor(distance);
	}
}

function getScore(distance) {
	var score = 0;
	var maxCircumference = 20039; // max kilometers along equator
	score = distance/maxCircumference;
	// closer results will have lowest difference, i.e. closer to 0, but the score = a confidence
	// level where 1 not 0 is high, so we need to reverse the figure by subtracting it from 1
	return (1-score);
}

// shortcut method for logging
function log(str, p1, p2) {
	if (DEBUG_MODE) {
		if (p2 != undefined) {
			util.log(util.format(str,p1,p2));
		} else if (p1 != undefined) {
			util.log(util.format(str,p1));
		} else {
			util.log(util.format(str));
		}
	}
}

module.exports.getFullCity = getFullCity;
module.exports.getDistance = getDistance;
module.exports.getScore = getScore;
module.exports.compare = compare;
module.exports.handleRequest = handleRequest;