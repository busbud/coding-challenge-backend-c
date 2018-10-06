var http = require('http');
var port = process.env.PORT || 2345;
//this class is used to store data read form the json file 
var City = function City(name, lon, lat, pop, pay){
	this.name = name;
	this.population = pop;
	this.longitude = lon;
	this.latitude = lat;
	this.country = pay;
	this.getScore = function (longitude, latitude){
		if(isNaN(longitude) || longitude == undefined ||isNaN(latitude) || latitude == undefined )
			return  Math.exp(-(10000/this.population));
		return Math.exp(-(Math.abs((longitude - this.longitude)/90))
										-(Math.abs((latitude - this.latitude)/90))
										-(1/this.population));
	}
}
//this class is used to store suggestions that will be returned as a result
var Suggestion = function Suggestion(name, lon, lat, score, pay){
	this.name = name + ", " + pay;
	this.score = score;
	this.longitude = lon;
	this.latitude = lat;
}
//this function loops through all the cities and comptues the score of every matching city
function applySearch(q, lon, lat, data){
	var suggestions = [];
	var nSuggestions = 0;
	for(var i = 0; i < data.length; i++){
		if(data[i].name.startsWith(q)){
			suggestions[nSuggestions++] = new Suggestion(data[i].name,data[i].longitude,data[i].latitude,data[i].getScore(lon, lat), data[i].country)
		}
	}
	
	return nSuggestions == 0 ? null : suggestions;
}

//loading the json file
var fs = require("fs");
var contents = fs.readFileSync("data/cities_canada-usa.json");
var json = JSON.parse(contents);

var data = [];
for(var i = 1; i < json.length; i++){
	data[i-1] = new City(json[i][2], parseFloat(json[i][5]), parseFloat(json[i][4]), parseInt(json[i][14]), json[i][8]);
}
//this function is used to compare two suggestions in order to sort them ascendingly by score 
function compareCities(s1, s2){
	return  s2.score - s1.score;
}

module.exports = http.createServer(function (req, res) {
	if (req.url.indexOf('/suggestions') === 0) {
		//valid HTTP GET request
		var splitted = req.url.split(/=|&/);
		var q = splitted[1];
		var lat = parseFloat(splitted[3]);
		var lon = parseFloat(splitted[5]);
		var result = applySearch(q, lon, lat, data);
		
		if(result === null){
			//no matches found
			res.writeHead(404, {'Content-Type': 'text/plain'});
			result = [];
		}
		else{
			//at least one match found
			res.writeHead(200, { 'Content-Type': 'text/plain' });
			result.sort(compareCities);
		}
		//send json response
		res.end(JSON.stringify({
			suggestions: result
			
		}));
	} else {
		//invalid HTTP GET request
		res.writeHead(404, {'Content-Type': 'text/plain'});
		res.end();
	}
  }).listen(port, () => {
    console.log("App is running on port " + port);
});
  