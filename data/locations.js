var mongoose = require('mongoose');
var locationObject = require('./location-schema');

mongoose.connect('mongodb://localhost/location-db', function(err) {
    if(err) {
        console.log('connection error', err);
    } else {
        console.log('connection to db successful');
    }
});
	//need to search for lat and longetitude if present
	//need to sanitize query input
	//locations.aggregate( [ { $match: { $text : { $search : queryString.q } } }, 
	//the query must at least start with the correct word.

var locations = {
    search : function(queryString, callback){
	//console.log(queryString.longitude + " " + queryString.latitude + " long and lat");
	locationObject.aggregate([ 
		{ $geoNear: {
		    near : { type: "Point", coordinates: [ parseFloat(queryString.longitude) ,  parseFloat(queryString.latitude) ] },
		    distanceField: "dist.calculated",
		    $maxDistance : 2,
		    spherical: true
	        }
		},
	    { $match: { 
		$or : [ { name : { $regex : new RegExp("^"+queryString.q), $options:'i' }}]
	    }},
	    { $sort: { 
		score: {$meta: "textScore" }, name: 1 }
	    },
	    { $project : { 
		"ascii" : 1, 
		"name" : 1,
		"country" : 1, 
		"coords" : 1,
		"admin1" : 1,
		"score" : 1}
	    }
	    ], function(err, locs){
	    if(err){
		console.log(err);
		callback(err,[]);
	    }
	    
	    else{
		console.log("everything cool");
		//console.log(locs[1]); //debugging
		callback(null,locs);
	    }
	});
    }
};

module.exports = locations;
