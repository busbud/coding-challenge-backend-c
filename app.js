var http = require('http');
var url = require('url');
var port = process.env.PORT || 2345;
var env = process.env.NODE_ENV || "development";
var MongoClient = require('mongodb').MongoClient;

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
				var lat, long;
				// error check for latitude
				if (parts.query['latitude'] != undefined && parts.query['latitude'].trim() != "") {
					if (!isNumeric(parts.query['latitude'])) {
						log("latitude argument must be numeric.");
					} else {
						lat = parts.query['latitude'];
					}
				}
				// error check for longitude
				if (parts.query['longitude'] != undefined && parts.query['longitude'].trim() != "") {
					if (!isNumeric(parts.query['longitude'])) {
						log("longitude argument must be numeric.");
					} else {
						long = parts.query['longitude'];
					}
				}
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
				                	var fullCountryName = (entry['country'] == 'US' ? "USA" : "Canada");
				                	var fullcity = entry['name'] + ", " + entry['admin1'] + ", " + fullCountryName;
				                	var score = getScore(q, entry);
				                	log('score for ' + entry['name'] + ' = ' + score);
				                	arrItem={"name":fullcity,
				                			"alt_name":entry['alt_name'],
				                			"latitude":entry['lat'],
				                			"longitude":entry['long'],
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

var isNumeric = function (obj) {
    obj = typeof(obj) === "string" ? obj.replace(",", ".") : obj;
    return !isNaN(parseFloat(obj)) && isFinite(obj) && Object.prototype.toString.call(obj).toLowerCase() !== "[object array]";
};

//convenience method
function log(str) {
	if (env == 'development') console.log(str);
}

function compare(a,b) {
  if (a.score < b.score)
     return 1;
  if (a.score > b.score)
    return -1;
  return 0;
}

function getScore(qSearchString, entry) {
	var score = 0;
	// RULE: if name only appears as an alternate, give a low score
	log('||')
	if (qSearchString.trim().toLowerCase() == entry['name'].trim().toLowerCase()) {
		score += .5;
	} else {
		score += .1;
	}
	return score;
}
log('Server running at http://127.0.0.1:' + port + '/suggestions');