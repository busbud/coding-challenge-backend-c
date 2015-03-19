var mongoose = require('mongoose');
var locationObject = require('./location-schema');

//Redis to go for heroku
if (process.env.REDISTOGO_URL) {
    var rtg   = require("url").parse(process.env.REDISTOGO_URL);
    var redisClient = require("redis").createClient(rtg.port, rtg.hostname);
    redisClient.auth(rtg.auth.split(":")[1]);
} else {
    var redisClient = require("redis").createClient();
}

mongoose.connect('mongodb://localhost/location-db', function(err) {
    if(err) {
        console.log('connection error', err);
    } else {
        console.log('connection to db successful');
    }
});


function createRegex(cityName){
    var regex = "";
    //too convoluted for what I wanted to do. With more time maybe allowing for mistakes/mispelling could
    //be handled more gracefully.
    /*for (var i = 1, len = cityName.length; i < len; i++) {
	regex = regex + '^' + cityName.substring(0,i) + '.' +  cityName.substring(i+1,len) + '|'; 
    }*/
    regex = regex + '^' + cityName;

    //remove last pipe
    //regex = regex.substr(0, regex.length - 1);
    //regex = regex + '/';
    return regex; 
}

function constructParams(queryString, params){
    var aggregates = [];
    //geoNear aggregate will sort the cities by proximity to the input (longitude,latitude) coordinates
    //makes use of the mongodb 2dsphere index for quick results over large data sets
    //keep only cities populated > 5000
    var geoNear = { 
	$geoNear: {
	    near : { type: "Point", coordinates: [ parseFloat(queryString.longitude) ,  parseFloat(queryString.latitude) ] },
	    distanceField: "dist.calculated",
	    spherical: true,
	    query : {population : { $gt : 5000 }},
	    limit : 100000
	}	
    };
    //match the query name against the generated regex. Regex deals only in ASCII.
    //TODO: have language option?
    var matchName =  { 
	$match: { 
	    ascii : { $regex : "filler", $options:'i'},
	    population : { $gt : 5000 },
	}};
    var limit = {
	$limit : 20
    };
    //project only the needed fields to the next aggregate stage
    var project = { $project : { 
	"name" : { $concat : ["$ascii" , ", " , {$substr : ["$admin1", 0, 2]}, ", " , "$country"] },
	"loc" : 1,
	"dist.calculated" : 1,
	"population" : 1,
	"_id" : 1,
	"ascii" : 1
    }};
    //push the geoNear stage if the user put in longitude/latitude.
    //limit results if no name was entered to 10 closest cities.
    if(queryString.longitude != undefined && queryString.latitude != undefined){
	if(queryString.q == undefined)
	    geoNear.$geoNear.limit = 10;
	aggregates.push(geoNear);
    }

    //generate the prefix regex match. Prefix makes use of the mongo db index.
    if(queryString.q != undefined){
	matchName.$match.ascii.$regex = createRegex(queryString.q);
	aggregates.push(matchName);
    }
    if(queryString.limit != undefined)
	limit.$limit = parseInt(queryString.limit);

    aggregates.push(limit,project);
    return aggregates;
}
//compute the score of both the geolocation and the name
//for geoscore we remove 0.1/100km.
//for the namescore, since I'm not implementing any spelling error and we're matching on the prefix
//all results returned will have high confidence
function computeScore(queryString, locationName, distance){
    var nameScore = 1;
    var geoScore = 1;
    var totalScore = 0;
    if(queryString.q != null){
	var common = locationName.replace(queryString.q, "");
	if(common.length == 0)
	    nameScore = 1;
	else
	    nameScore = 0.9;
	totalScore +=nameScore;
    }
    if(queryString.longitude != null && queryString.latitude != null){
	geoScore = 1 - (distance.calculated / 1000000)
	totalScore += geoScore;
    }
    if(queryString.q != null && queryString.longitude != null && queryString.latitude != null)
	return (totalScore / 2);
    return totalScore;
}

//first check redis in memory for the queried term. If there is no result, we go to mongo.
//ideally, redis would sit on a server different than mongo since they have conflicting approaches for using 
//memory. (mongo scales well with OS memory-swapping but redis does not)
var locations = {
    search : function(queryString, callback){
	//query redis first
	redisClient.get("query_" + JSON.stringify(queryString), function(err, redisResults) {
	    if( err || !redisResults ){
		locationObject.aggregate(constructParams(queryString, null), function(err, locs){
		    if(err){
			console.log(err);
			callback(err,[]);
		    }		    
		    else{
			//compute score. It would have been nice to do this in the aggregate stage but mongo
			//doesn't deal well with string operations inside aggregate.
			var filteredLocs = [];
			locs.forEach(function(doc){
			    doc.score = computeScore(queryString, doc.ascii, doc.dist);
			    if(doc.score > 0){
				filteredLocs.push(doc);
			    }
			});
			//cache result into redis. Store only temporarily
			redisClient.setex("query_" + JSON.stringify(queryString), 21600, JSON.stringify(filteredLocs, null, 2));
			callback(null,filteredLocs);
		    }
		});		
	    }
	    else{
		//parse results back into json since redis stores values as strings
		callback(null, JSON.parse(redisResults));
	    }
	});
    }};

module.exports = locations;
