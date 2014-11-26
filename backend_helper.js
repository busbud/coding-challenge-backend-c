var MIN_POPULATION = 5000;

// Computes the score based on how close the query match is
// Includes how close the latitude and longitude are to the requested longitude and latitude as well
var getScore = function(regexp, name, longitude, latitude, q, lon, lat) {
	var match = regexp.exec(name);
	var name_score = 0.0;
	if(match)
		name_score = q.length/match[0].length;
	var longitude_score = 1;
	var latitude_score = 1; 
	if(lon && !isNaN(lon))
		longitude_score = 1 - Math.abs(longitude-(lon%180))/180;
	if(lat && !isNaN(lat))
		latitude_score = 1 - Math.abs(latitude-(lat%180))/180;
	return name_score;
	return Math.min(1,((name_score + longitude_score + latitude_score)/3).toFixed(2));
};

// Format the suggestions, compute their score based on given inputs
// Then sort them by score
exports.makeSuggestions = function(data, q, longitude, latitude) {
	var regexp = new RegExp(q, 'i');
	var results = [];
	var lines = data.split('\n');
	var city;
	for(var i=0; i<lines.length; i++) {
		if(regexp.test(lines[i])) {
			city = lines[i].split('\t');
			if(city[14] >= MIN_POPULATION) {
				results.push({
					name: city[1]+", "+city[8],
	  				longitude: city[4],
	  				latitude: city[5],
	  				score: getScore(regexp, city[1], city[4], city[5], q, longitude, latitude)
				});
			}
		}
	}
	return results.sort(function(a,b) {
  		return b.score - a.score;
  	});
}