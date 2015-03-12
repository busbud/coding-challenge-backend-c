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
	locationObject.aggregate( [ {$match: { $or : [ { name : { $regex : new RegExp("^"+queryString.q), $options:'i' } }, {lat : {$gt : 0, $lt : 100}}] } }, 
			       { $sort: { score: { $meta: "textScore" }, name: 1 } },
			       { $project : { "ascii" : 1, 
					    "name" : 1, 
					    "country" : 1, 
					    "lat" : 1, 
					    "longitude" : 1,
					    "admin1" : 1,
					    "score" : 1}}], function(err, locs){
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
