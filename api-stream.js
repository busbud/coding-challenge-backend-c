var fs = require('fs');
var filePath = 'data/cities.json';
var util = require('util'); 
var common = require('./common'); 
// var sleep = require('sleep');

var dataIsReady = false;
var buf = '';
// the all important object containing our data, an array of arrays
var objCities = {};
numCities = 0;

function loadData() {
	var startTime = new Date().getTime();
	log('Opening stream to file %s', filePath);
	var stream = fs.createReadStream(filePath, {flags: 'r', encoding: 'utf-8'});
	stream.on('data', function(d) {
	    buf += d.toString(); // when data is read, stash it in a string buffer
	    pump(); // then process the buffer
	});
	stream.on('close', function(d) {
		dataIsReady = true;
		var endTime = new Date().getTime();
		var executionTime = endTime - startTime;
		log('Stream closed. %d cities loaded into memory after %d milliseconds.', numCities, executionTime);
	});
}

function pump() {
    var pos;
    while ((pos = buf.indexOf('\n')) >= 0) { // keep going while there's a newline somewhere in the buffer
        if (pos == 0) { // if there's more than one newline in a row, the buffer will now start with a newline
            buf = buf.slice(1); // discard it
            continue; // so that the next iteration will start with data
        }
        process(buf.slice(0,pos)); // hand off the line
        buf = buf.slice(pos+1); // and slice the processed data off the buffer
    }
    log('Loading data. %s cities created.', numCities);
}

function searchCities(str, arrSuggestedCities) {
	// only search if data is ready
	if (dataIsReady) {
		// ensure is lower case
		str = str.toLowerCase();
	    for (var key in objCities) {
			var city = objCities[key];
			// if matches either name field, add to our array of suggested cities
			if (city.name.startsWith(str) || city.ascii.startsWith(str)) {
				// if we just push the city, we are only adding a pointer to the static data.
				// we want a deep copy which can be modified independently.
				// thus we need to use this JSON parse & stringify
				arrSuggestedCities.push(JSON.parse(JSON.stringify(city)));
			}
	    }
    } else{
    	log('Data not yet loaded to perform search.');
    }
}

function showCityData() {
    for (var key in objCities) {
    	log('\t' + key + '=' + objCities[key]);
        for (var subkey in objCities[key]) {
        	log('\t\t' + subkey + '=' + objCities[key][subkey]);
        }
    }
}

function process(line) {
    if (line[line.length-1] == '\r') line=line.substr(0,line.length-1); // discard CR (0x0D)
    if (line.length > 0) { // ignore empty lines
        var city = JSON.parse(line); // parse the JSON
        // remove fields we don't need
        delete city.tz;
        delete city.elevation;
        delete city.modified_at;
        delete city.dem;
        delete city.admin2;
        delete city.admin3;
        delete city.admin4;
        delete city.feat_class;
        delete city.feat_code;
        delete city.cc2;
        delete city.population;
        delete city.alt_name;
        delete city.id;
        delete city._id;
        // remap the lat & long names
        city.latitude = city.lat;
        delete city.lat;
        city.longitude = city.long;
        delete city.long;
        // create new ID using format name + state/province + country
        var uniqueName = city.name + '|' + city.admin1 + '|' + city.country
        // clean the data once at script runtime
        // first we assemble the display name
        city.displayName = common.getDisplayName(city);
        // then we lower-case the name fields for smooth searches later
        city.name = city.name.toLowerCase();
        city.ascii = city.ascii.toLowerCase();
        // now we can delete other fields we no longer need
        delete city.country;
        delete city.admin1;
        // add to objCities with uniqueName
        objCities[uniqueName] = city;
        numCities++;
    }
}

function log(str, p1, p2) { 
	common.log(str, p1, p2); 
}

function handleRequest(req, res, parts) {
	var startTime = new Date().getTime();
	log(parts.query);
	var qCity = parts.query['q'];
	// create temp array for this request
	var suggestedCities = [];
	// copy matched items into our array
	searchCities(qCity, suggestedCities);
    log('%d results returned for %s', suggestedCities.length, qCity);
	// modify the city data for presentation, adding scored results
    prepareCityData(suggestedCities, parts);
    // sort the results by score
    var sortedResults = suggestedCities.sort(common.compare);
    // compute time to handle request
	var endTime = new Date().getTime();
	var executionTime = endTime - startTime;
	log('Response execution time: %d milliseconds.', executionTime);
    // send JSON response
	res.status(404).end(JSON.stringify({'suggestions':sortedResults}) + '\n');
}

function prepareCityData(arr, parts) {
	// need to re-map the fields to match those of the API
    for (x=0; x<arr.length; x++) {
		var city = arr[x];
		// add distance
		city.distance = common.getDistance(parts, city.latitude, city.longitude);
		// add score based on distance
		city.score = common.getScore(city.distance);
		// map displayname to name
		city.name = city.displayName;
		delete city.displayName;
		// remove ascii
		delete city.ascii
    }
}

//if the page is called directly, assume we are testing
// test();
function test() {
	log('page loaded');
	loadData();
}

module.exports.loadData = loadData;
module.exports.handleRequest = handleRequest;

