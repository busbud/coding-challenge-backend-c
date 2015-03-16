var mongoose = require('mongoose');
var locationObject = require('./location-schema');
var redisClient = require('redis').createClient();


mongoose.connect('mongodb://localhost/location-db', function(err) {
    if(err) {
        console.log('connection error', err);
    } else {
        console.log('connection to db successful');
    }
});

//allow match on 1 extra character or 1 missing character.
//also allow match on 1 mispelled character
//except first letter of the input. Too hard to disambiguate between londo and hondo when user
//types in Londo. He obviously meant Londo cause who wants to go to Hondo?
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
    console.log(regex);
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
    var matchScore = {$match : {
	score : {$gte : 0}
    }};
    //project only the needed fields to the next aggregate stage
    var project = { $project : { 
	"name" : { $concat : ["$ascii" , ", " , {$substr : ["$admin1", 0, 2]}, ", " , "$country"] },
	"loc" : 1,
	"dist.calculated" : 1,
	"population" : 1,
	"_id" : 1
    }};
    var projectScore = { $project : { 
	"name" : 1,
	"loc" : 1,
	"dist.calculated" : 1,
	"population" : 1,
	"_id" : 1,
	"geoscore" : 1,
	"namescore" : 1
    }};
    var sort = {$sort : {
	"score" : -1
    }};

    //remove (0.1 confidence) / 100 km. (distance is in meters so divide by 1000 also)
    var computeGeoscore =  { $subtract : [ 1 , { $divide : [ "$dist.calculated", 1000000]}]};
    var computeNamescore = { $subtract : [ 1, {$divide : [ {$strcasecmp : ["$ascii", queryString.q]}, 10]}]};

    //push the geoNear stage if the user put in longitude/latitude.
    //limit results if no name was entered to 10 closest cities.
    //add the computed geoscore to the projected fields
    if(queryString.longitude != null && queryString.latitude != null){
	if(queryString.q == null)
	    geoNear.$geoNear.limit = 10;
	project.$project.geoscore = computeGeoscore;
	aggregates.push(geoNear);
    }
    //add the computed namescore to the project fields
    //generate the prefix regex match. Prefix makes use of the mongo db index.
    if(queryString.q != null){
	matchName.$match.ascii.$regex = createRegex(queryString.q);
	project.$project.namescore = computeNamescore;
	aggregates.push(matchName);
    }
    //if just query name
    if(queryString.q != null && queryString.longitude == null && queryString.latitude == null)
	projectScore.$project.score = "$namescore";
    //if just longitude/latitude
    if(queryString.q == null && queryString.longitude != null && queryString.latitude != null)
	projectScore.$project.score = "$geoscore";
    //if both query name and longitude/latitude. Give equal weight to both though adding weights might be 
    //a good idea
    else if(queryString.q != null && queryString.longitude != null && queryString.latitude != null){
	projectScore.$project.score = {$divide : [{$add : ["$namescore", "$geoscore"]}, 2] };
    }

    aggregates.push(project, projectScore, matchScore, sort);
    return aggregates;
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
			//cache result into redis. Store only temporarily
			//redisClient.setex("query_" + JSON.stringify(queryString), 21600, JSON.stringify(locs, null, 2));
			callback(null,locs);
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
