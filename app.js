var http = require('http');
var url = require('url');
var port = process.env.PORT || 2345;
var env = process.env.NODE_ENV || "development";
var MongoClient = require('mongodb').MongoClient;

var isNumeric = function (obj) {
    obj = typeof(obj) === "string" ? obj.replace(",", ".") : obj;
    return !isNaN(parseFloat(obj)) && isFinite(obj) && Object.prototype.toString.call(obj).toLowerCase() !== "[object array]";
};

//convenience method
function log(str) {
	if (env == 'development') console.log(str);
}

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
				    	alt_name:regex,
				    	population:{$gt: popLimit}
				    }
				    ,{ name:1, country:1, admin1:1, lat:1, long:1, _id:0 }	
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
				                	var fullcity = entry['name'] + " - " + entry['admin1'] + " - " + fullCountryName;
				                	arrItem={"name":fullcity,
				                			"latitude":entry['lat'],
				                			"longitude":entry['long'],
				                			"score":0.0
				                		};
				                	arrResults.push(arrItem);
				                });
								resp.writeHead(200, {'Content-Type':'application/json'});
								resp.end(JSON.stringify({"suggestions":arrResults}) + '\n');
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
	}).listen(port, '127.0.0.1');
} catch (e) {
	log("oops");
}

log('Server running at http://127.0.0.1:' + port + '/suggestions');