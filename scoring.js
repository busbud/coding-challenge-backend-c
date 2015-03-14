// Scoring will be based on the optional inputs, longitude and latitiude, and the population
// of the city. The weighting will be 0.7 distance and 0.3 population when longitude and 
// latitiude are inputed and 1.0 population when they are not

var populationWeight = 0.3;
var distanceWeight = 0.7;

exports.getScore = function (population, longitude, latitude, inputLongitude, inputLatitude) {
	var score = 0;
	if (inputLongitude === undefined && inputLatitude === undefined) {
		score = scoreByPopulation(population);
	} else {
		score = populationWeight*scoreByPopulation(population) + distanceWeight*scoreByLocation(longitude, latitude, inputLongitude, inputLatitude);
	}

	//round to 2 decimal places
	return Math.round(score * 100) / 100;

}


// Populations of greater than 90 000 receive a score of 1.0 and all
// other cities receive a score based on their value between 0 and 90 000
var scoreByPopulation = function (population) {
	population += 10000;
	if (population > 100000) {
		return 1.0;
	}
	 else {
		return population/100000;
	}
}


// Cities with a distance of less than 20 km receive a score of 1.0. Cities with
// a distance of greter than 2000 km receive a score of 0.0. All other cities
// are distributed in between 0 and 1 based on the distance.
var scoreByLocation = function (longitude, latitude, inputLongitude, inputLatitude) {
	var distance = haversineDistance(longitude, latitude, inputLongitude, inputLatitude);
	if (distance < 20) {
		return 1.0;
	} else if (distance > 2000) {
		return 0.0;
	} else {
		return (1-(distance/2000));
	}
};

var haversineDistance = function (lon1, lat1, lon2, lat2) {
	var x1 = lat2 - lat1;
	var x2 = lon2 - lon1;

	var dLat = toRad(x1);
	var dLon = toRad(x2);

	var R = 6371;
	var a = Math.sin(dLat/2) * Math.sin(dLat/2) + 
				Math.cos(toRad(lat1)) * Math.cos(toRad(lat2)) *
				Math.sin(dLon/2) * Math.sin(dLon/2);

	var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));

	return R * c;
};

var toRad = function (a) {
	return a * Math.PI / 180;
};
