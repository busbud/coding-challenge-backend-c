var mongoose = require('mongoose');
var locationObject = require('./location-schema');

mongoose.connect('mongodb://localhost/location-db', function(err) {
    if(err) {
        console.log('connection error', err);
    } else {
        console.log('connection to db successful');
    }
});

//allow match on 1 extra character or 1 missing character.
//also allow match on 1 modified character
//and finally match on any two letters interchanged
//allowing any more than that will return erroneous results...if the user types in Shanghai
//he shouldnt expect to see Berlin in the suggestions.

function createRegex(cityName){
    var regex = "/";

    for (var i = 0, len = cityName.length; i <= len; i++) {
	regex = regex + '^' + cityName.substring(0,i) + '.' + cityName.substring(i,len) + '|';
    }

    for (var i = 0, len = cityName.length; i <= len; i++) {
	//allow
	regex = regex + '^' + cityName.substring(0,i) + '.' +  cityName.substring(i+1,len) + '$|'; 
	regex = regex + '^' +  cityName.substring(0,i) +  cityName.substring(i+1,len) + '$|' ;
	regex = regex + '^' +  cityName.substring(0,i) +  cityName.substring(i+1,i+2) +  cityName.substring(i,i+1) +  cityName.substring(i+2, len) + '$';
    }
    regex = regex + "/";
    console.log(regex);
    return regex;
    
}

function constructParams(queryString, params){
    var aggregateParams = [];
    if(queryString.longitude != null && queryString.latitude != null){
	console.log("long/lat sent");
	aggregateParams.push({ 
	    $geoNear: {
		near : { type: "Point", coordinates: [ parseFloat(queryString.longitude) ,  parseFloat(queryString.latitude) ] },
		distanceField: "dist.calculated",
		spherical: true

	    }
	}
		      );
    }
    if(queryString.q != null){
	aggregateParams.push( 
	    { $match: { 
		name : { $regex : createRegex(queryString.q), $options:'i' },
		population : { $gt : 5000 }
	    }},
	    { $sort: { 
		score: {$meta: "textScore" }, name: 1 }
	    },
	    { $project : { 
		"ascii" : 1, 
		"name" : 1,
		"country" : 1, 
		"loc" : 1,
		"admin1" : 1,
		"score" : 1,
		"dist.calculated" : 1,
		"population" : 1
	    }
	    });
    }
    return aggregateParams;
}

var locations = {
    search : function(queryString, callback){
	//console.log(queryString.longitude + " " + queryString.latitude + " long and lat");
	locationObject.aggregate(constructParams(queryString, null), function(err, locs){
	    if(err){
		console.log(err);
		callback(err,[]);
	    }
	    
	    else{
		console.log("successfull query execution");
		//console.log(locs[1]); //debugging
		callback(null,locs);
	    }
	});
    }
};

module.exports = locations;
