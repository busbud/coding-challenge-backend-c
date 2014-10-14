var url = require('url');
var MongoClient = require('mongodb').MongoClient;
var common = require('./common'); 
var MONGO_URI = process.env.MONGOLAB_URI || 'mongodb://localhost:27017/busbud' // mongodb://dbuser:dbpassword@host:port/dbname

function handleRequest(req, res, parts) {
	var startTime = new Date().getTime();
	var arrResults = [];
	log(parts.query);
	var q = parts.query['q'];
	MongoClient.connect(MONGO_URI, function(err, db) {
		if(err) { 
			log('Connection error to %s:%s', MONGO_URI, err);
		}
	    var collection = db.collection('cities');
	    var popLimit = 5000;
	    var regex = new RegExp('^'+q, 'i')
	    collection.find({
	    	$or: [ { ascii:regex }, { name:regex } ] 
	    	// don't really need the line below since our TSV dataset is already truncated to population > 5000,
	    	// but the filter would be required when a full city data set
	    	,population:{$gt: popLimit} 
	    }
	    ,{ name:1, alt_name:1, ascii:1, country:1, admin1:1, lat:1, long:1, _id:0 }	// filter for required fields
	    ).sort({name:1}).toArray(function(err, results) {
	    	if (err != null) {
	    		log('Error finding result: %s', err);
	    	} else {
	            if (results == undefined || results.length == 0) {
	                log('0 results returned for %s', q);
	            	res.status(404).end(JSON.stringify({'suggestions':arrResults}) + '\n');
	            } else {
	                log('%d results returned for %s', results.length, q);
	                results.forEach(function(entry) {
		                var fullcity = common.getDisplayName(entry);
		                var distance = common.getDistance(parts, entry['lat'], entry['long']);
		                var score = common.getScore(distance);
		                var distance = common.getDistance(parts, entry['lat'], entry['long']);
		                arrItem={'name':fullcity,
		                	'latitude':entry['lat'],
		                	'longitude':entry['long'],
		                	'score':score,
							'distance':distance // included for reference and debugging
		                };
		                arrResults.push(arrItem);
	                });
	                // compute time to handle request
	            	var endTime = new Date().getTime();
	            	var executionTime = endTime - startTime;
	            	log('Response execution time: %d milliseconds.', executionTime);
	                // present sorted results
					res.status(200).end(JSON.stringify({'suggestions':arrResults.sort(common.compare)}) + '\n');
	                db.close();
	            }
	    	}
	    });
	});	
}

function log(str, p1, p2) { 
	common.log(str, p1, p2); 
}

module.exports.handleRequest = handleRequest;