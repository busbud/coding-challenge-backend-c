// Computes the score based on how close the query match is
// Includes how close the latitude and longitude are to the requested longitude and latitude as well
exports.getScore = function(name, longitude, latitude, q, lon, lat) {
	var name_score = 1 - Math.abs(name.length-q.length)/name.length;
	var longitude_score = 1;
	var latitude_score = 1; 
	if(lon && lat) {
		longitude_score = 1 - Math.abs(longitude-lon)/longitude;
		latitude_score = 1 - Math.abs(latitude-lat)/latitude;
	};
	return (name_score + longitude_score + latitude_score)/3;
};