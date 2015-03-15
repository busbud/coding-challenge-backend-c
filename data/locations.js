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
	regex = regex + '^' + cityName.substring(0,i) + '.' +  cityName.substring(i+1,len) + '$|'; 
	regex = regex + '^' +  cityName.substring(0,i) +  cityName.substring(i+1,len) + '$|' ;
	regex = regex + '^' +  cityName.substring(0,i) +  
	    cityName.substring(i+1,i+2) +  
	    cityName.substring(i,i+1) +  
	    cityName.substring(i+2, len) + '$';
    }
    regex = regex + "/";
    return regex;
    
}

function constructParams(queryString, params){
    var aggregates = [];
    var geoNear = {$geoNear: {
	near : { type: "Point", coordinates: [ parseFloat(queryString.longitude) ,  parseFloat(queryString.latitude) ] },
	distanceField: "dist.calculated",
	spherical: true,
	query : {population : { $gt : 5000 }},
	limit : 100000
	}	
    };
    var sort =  { $sort: { 
	score: { $meta: "textScore" } , name : 1,
    }};
    var project =   { $project : { 
	"ascii" : 1, 
	"country" : 1, 
	"loc" : 1,
	"admin1" : 1,
	"dist.calculated" : 1,
	"population" : 1,
	"_id" : 0,
	"score" : {$literal : 0.1},
	"name" : { $concat : ["$ascii" , ", " , {$substr : ["$admin1", 0, 2]}, ", " , "$country"] }
    }};

    if(queryString.longitude != null && queryString.latitude != null){
	aggregates.push(geoNear, sort);
    }

    if(queryString.q != null){
	var regex = createRegex(queryString.q);
	var match =  { $match: { 
	    $or : [
		{name : { $regex : regex, $options:'i' }},
		{ascii : { $regex : regex, $options:'i'}}
	    ]
	    ,
	    population : { $gt : 5000 }
	}};
	aggregates.push(match, sort)
    }
    aggregates.push(project);
    return aggregates;
}

var locations = {
    search : function(queryString, callback){
	locationObject.aggregate(constructParams(queryString, null), function(err, locs){
	    if(err){
		console.log(err);
		callback(err,[]);
	    }
	    
	    else{
		callback(null,locs);
	    }
	});
    }
};

module.exports = locations;

