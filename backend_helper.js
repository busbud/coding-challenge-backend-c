exports.MIN_POPULATION = 5000;

// Computes the score based on how close the query match is
// Includes how close the latitude and longitude are to the requested longitude and latitude as well
exports.getScore = function(name, longitude, latitude, q, lon, lat) {
	var name_score = 1 - Math.abs(name.length-q.length)/name.length;
	var longitude_score = 1;
	var latitude_score = 1; 
	if(lon && !isNaN(lon))
		longitude_score = 1 - Math.abs(longitude-(lon%180))/180;
	if(lat && !isNaN(lat))
		latitude_score = 1 - Math.abs(latitude-(lat%180))/180;
	return Math.min(1,((name_score + longitude_score + latitude_score)/3).toFixed(3));
};

// Computes the matches between the input query argument and cities in the data
// Filters out the cities with population below minimum threshold
// as well as cities that do not match the input string
exports.getMatches = function(data, q) {
	return data.map(function(line) {
		return line.split('\t');
  	}).filter(function(city) {
  		return city[14] >= exports.MIN_POPULATION && city[1].indexOf(q) === 0;
  	});
}

// Format the suggestions, compute their score based on given inputs
// Then sort them by score
exports.makeSuggestions = function(cities, q, longitude, latitude) {
	return cities
	.map(function(city) {
		return {
			name: city[1]+", "+city[8],
  			longitude: city[4],
  			latitude: city[5],
  			score: exports.getScore(city[1], city[4], city[5], q, longitude, latitude)
  		};
  	})
  	.sort(function(a,b) {
  		return b.score - a.score;
  	});
}