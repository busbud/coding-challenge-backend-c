var http = require('http');
var fs = require('fs');
var stream = require('stream');
var readline = require('readline');

var filepath = './data/cities_canada-usa.tsv';
var port = process.env.PORT || 2345;
var provinces = {	// A list of provinces and how they relate to their admin1 column's value
		'01':'AB',	// Alberta
		'02':'BC',	// British Columbia
		'03':'MB',	// Manitoba
		'04':'NB',	// New Brunswick
		'13':'NT',	// Northwest Territories
		'07':'NS',	// Nova Scotia
		'14':'NU',	// Nunavut
		'08':'ON',	// Ontario
		'09':'PE',	// Prince Edward Island
		'10':'QC',	// Quebec
		'11':'SK',	// Saskatchewan
		'12':'YT',	// Yukon
		'05':'NL'	// Newfoundland and Labrador
};
var radiusOfEarth = 6371; // Approximately
var circOfEarth = 20015; // Technically this is half

module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
	  var url = require('url');
	  var url_parts = url.parse(req.url, true);
	  var query = url_parts.query;
	  
	  // Process the request. Get suggestions and score results
	  processReq(res, query['q'], query['latitude'], query['longitude'], sendResponse);
    
  } else {
    res.end();
  }
}).listen(port);

console.log('Server running at port %d/suggestions', port);

// Process a unique HTTP request and return a set of suggestion
function processReq(res, q, latitude, longitude, callback) {
	var sugObj = {
			query: q,
			latitude: latitude,
			longitude: longitude,
			suggestions : []  
	  };
	var is = fs.createReadStream(filepath);
	var os = new stream();
	os.readable = true;
	os.writable = true;
	var iteration = 0, header = [], buffer = "";
	
	var reader = readline.createInterface({
		input: is,
	    output: os,
	    terminal: false
	});
	
	// Read each line in from the input file
	reader.on('line', function(line) {
		if(iteration++ == 0) {
			header = line.split('\t');
		} else {
			
			// Build the record
			var record = buildRecord(line);
			
			// do not add a record with population less than 5000 (that's what null means in this case)
			if (record != null) {
				
				// Score and determine whether or not to include the record in the suggestions
				if (scoreRecord(record)) {
					addRecord(record);
				}
				
			}
			
			
		}
	});
	
	// Close event, we want to call our callback and send a response
	reader.on('close', function() {
		callback(res, sugObj, q);
	});
	
	// Build a record from a line from the input file
	function buildRecord(str) {
		var record = {};
		var name = '', stateProv = '', country = '';
		var population = 0;
		record['name'] = '';
		
		// Split each line up by the tabs that separate each value
	    str.toString().split('\t').forEach(function(value, index){
	    	
	    	if(header[index] == 'ascii') {
	    		
	    		// Capture the ascii value of the city's name
	    		name = value;
	    		
	    	} else if (header[index] == 'admin1') {
	    		
	    		// This denotes the first division of a region (state or province in this case)
	    		stateProv = value;
	    		
	    	} else if (header[index] == 'country') {
	    		
	    		// Get the city's country and lengthen the name
	    		if (value.toLowerCase() == 'ca') {
	    			country = 'Canada';
	    		} else if (value.toLowerCase() == 'us') {
	    			country = 'USA';
	    		}
	    		
	    	} else if (header[index] == 'lat') {
	    		
	    		// Set the latitude of the city
	    		record['latitude'] = value;
	    		
	    	} else if (header[index] == 'long') {
	    		
	    			// Set the longitude of the sity
		    		record['longitude'] = value;
		    		
		    } else if (header[index] == 'population') {
		    	
		    	// Restrict population to by 5000 or more
	    		population = parseInt(value);
	    		if (population < 5000) {
	    			return null;
	    		}
		    }
	    });
	    
	    // If the country is Canada, dereference the province's number to its name
	    if (country == 'Canada') {
	    	stateProv = provinces[stateProv.toString()];
	    }
	    
	    // Create the full name record
	    record['name'] = name + ', ' + stateProv + ', ' + country;
	    
	    // default score is 0
	    record['score'] = 0;
	    
	    return record;
	}
	
	// Add a record to our suggestion set
	function addRecord(record) {
		sugObj.suggestions.push(record);
	}
	
	// Calculate a score for a record
	/*
	 * Details:
	 * There are two distinct scoring algorithms used in the calculation of a city's score:
	 * 		- A modified Levenshtein distance algorithm for matching city names
	 * 		- A distance formula for how close a city is to a given latitude and longitude
	 * 
	 * If nothing but a city name query is provided, only the Levenshtein score is used
	 * If nothing but a latitude and longitude pair is provided, only the physical distance score is used
	 * If both of the obove are provided then the two scores are weighted equally and combined 
	 * 
	 * The Levenshtein is modified to only count distance up until the shortest of two string ends.
	 * The distance is then subtracted from a weighted length of the city's name then divided by the length of the city's name
	 * 
	 * 			score = ( length(city_name) * weight - levenshteinDistance(query_string, city_name) ) / length(city_name)
	 * 
	 * where the weight is calculated by
	 * 			min( length(query_string), length(city_name) ) / max( length(query_string), length(city_name) )
	 * 
	 * This is done to eliminate scores of 1 when a query string is a substring of a city name (starting from the beginning only)
	 * 
	 * The physical distance calculation is done by calculating the distance in kilometers between two points and then subtracting
	 * that distance from half of the circumference of the earth, then diving the result by the circumference of the earth.
	 * The result is a number between 0 and 1 where 1 indicates that two sets of latitudes and longitudes are equal, and 0 indicates
	 * that the two points are on exact opposite points of a sphere (that is earth).
	 */
	function scoreRecord(record) {
		
		if (isNumber(latitude) && isNumber(longitude)) {
			var latA = (latitude * 180) / Math.PI;
			var lonA = (longitude * 180) / Math.PI;
			var latB = (record['latitude'] * 180) / Math.PI;
			var lonB = (record['longitude'] * 180) / Math.PI;
			
			// The distance between two points in kilometers
			var aob = (Math.cos(latA)*Math.cos(latB)*Math.cos(lonB - lonA) + Math.sin(latA)*Math.sin(latB));
			var distance = Math.acos(aob) * radiusOfEarth;
			var proximityScore = (circOfEarth - distance) / circOfEarth;
			
			// If true, we need to combine two scores. Distance and name matched
			if (q != null) {
				var multiplier = 1;
				if (q.length < record['name'].length) {
					multiplier = q.length / record['name'].length;
				} else {
					multiplier =  record['name'].length / q.length;
				}
				
				var similarityScore = (record['name'].length * multiplier - levenshteinDistanceIterative(q.toLowerCase(), record['name'].toLowerCase())) / record['name'].length;
				if (similarityScore < 0) {
					similarityScore = 0;
				}
				
				record['score'] = proximityScore * 0.5 + similarityScore * 0.5;
			} else {
				record['score'] = proximityScore;
			}
			
		} else if (q != null) {
			var multiplier = 1;
			if (q.length < record['name'].length) {
				multiplier = q.length / record['name'].length;
			} else {
				multiplier =  record['name'].length / q.length;
			}
			record['score'] = (record['name'].length * multiplier - levenshteinDistanceIterative(q.toLowerCase(), record['name'].toLowerCase())) / record['name'].length;
			if (record['score'] < 0) {
				record['score'] = 0;
			}
		}
		
		return record['score'] >= 0.25;
	}
	
	function isNumber(n) {
		return !isNaN(parseFloat(n)) && isFinite(n);
	}
}


// Send the response back 
function sendResponse(res, sugObj, q) {
	
	sugObj.suggestions = mergeSort(sugObj.suggestions);
	
	var statusCode = 200;
	
	// No results, send 404 response code
	if (sugObj.suggestions.length == 0) {
		statusCode = 404;
	}
	
	res.writeHead(statusCode, { 'Content-Type': 'application/json'});
	
	res.write(JSON.stringify(sugObj));
	res.end();
}

// Calculate the distance between two strings
// This is a modified version of the Levenshtein algorithm taken from Wikipedia
// It is modified to ignore the difference in string lengths
function levenshteinDistanceIterative(s, t)
{
    // degenerate cases
    if (s == t) return 0;
    if (s.length == 0) return t.length;
    if (t.length == 0) return s.length;
    
    var minLength = 0;
    if (s.length < t.length) {
    	minLength = s.length;
    } else {
    	minLength = t.length;
    }
 
    // create two work vectors of integer distances; minLength was t.length in both
    var v0 = new Array(minLength + 1);
    var v1 = new Array(minLength + 1);
 
    // initialize v0 (the previous row of distances)
    // this row is A[0][i]: edit distance for an empty s
    // the distance is just the number of characters to delete from t
    for (var i = 0; i < v0.length; i++)
        v0[i] = i;
 
    for (var i = 0; i < minLength; i++) // minLength was s.length
    {
        // calculate v1 (current row distances) from the previous row v0
 
        // first element of v1 is A[i+1][0]
        //   edit distance is delete (i+1) chars from s to match empty t
        v1[0] = i + 1;
 
        // use formula to fill in the rest of the row
        for (var j = 0; j < minLength; j++) // minLength was t.length
        {
            var cost; // = (s[i] == t[j]) ? 0 : 1;
            if ((s[i] == t[j])) {
            	cost = 0;
            } else {
            	cost = 1;
            }
            v1[j + 1] = minimum(v1[j] + 1, v0[j + 1] + 1, v0[j] + cost);
        }
 
        // copy v1 (current row) to v0 (previous row) for next iteration
        for (var j = 0; j < v0.length; j++)
            v0[j] = v1[j];
    }

    return v1[minLength]; // minLength was t.length
}

// Find the minimum of 3 numbers
function minimum(i1, i2, i3) {
	if (i1 <= i2 && i1 <= i3) {
		return i1;
	} else if (i2 <= i1 && i2 <= i3) {
		return i2;
	} else if (i3 <= i1 && i3 <= i2) {
		return i3;
	} else {
		return i1;
	}
}

// The merge operation of merge sort
function merge(left, right){
    var result  = [],
        il      = 0,
        ir      = 0;

    while (il < left.length && ir < right.length){
        if (left[il]['score'] > right[ir]['score']){
            result.push(left[il++]);
        } else {
            result.push(right[ir++]);
        }
    }

    return result.concat(left.slice(il)).concat(right.slice(ir));
}

// Merge sort algorithm taken from www.nczonline.net
function mergeSort(items){

    if (items.length < 2) {
        return items;
    }

    var middle = Math.floor(items.length / 2),
        left    = items.slice(0, middle),
        right   = items.slice(middle),
        params = merge(mergeSort(left), mergeSort(right));
    
    // Add the arguments to replace everything between 0 and last item in the array
    params.unshift(0, items.length);
    items.splice.apply(items, params);
    return items;
}
