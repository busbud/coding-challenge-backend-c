//first node.js project 

var http = require('http');
var url = require('url');
var fs = require('fs');

//Used for 'fuzzy' search strings for city names
var lvn = require('js-levenshtein');

var port = process.env.PORT || 2345;

// Load the data into an array of JSON objects 
fs.readFile('data/cities_canada-usa.tsv', function (err, data) {
    if (err) throw err;
    var array = data.toString().split('\n');
    objContainer = [];
    var mapArray = array[0].split('\t');

    for (var i = 1; i < array.length; i++) {
        //Zip the header and subsequent lines into an Object

        var curObj = array[i].split('\t');
        var newObj = {};
        mapArray.forEach((key, index) => newObj[key] = curObj[index]);
        objContainer.push(newObj);
    }
});

module.exports = http.createServer(function (req, res) {
    if (req.url.indexOf('/suggestions') === 0) {
        res.writeHead(200, { 'Content-Type': 'text/plain' });
        query = url.parse(req.url, true).query;

        //Parse the query
        lookupCity = query.q;
        location = [query.longitude, query.latitude];

        top10Items = [];

        //If no location given and the city name is a valid then only use the city name to perform
        // the search, return the top 10 results
        if (lookupCity != "" && lookupCity != undefined && location.includes(undefined)) {

            // Use the levenshtein function as a comparator key
            top10Items = objContainer.sort(function (obj1, obj2) {

                if (obj1.ascii === undefined) {
                    return -1;
                } else if (obj2.ascii === undefined) {
                    return 1;
                }

                comp1 = lvn(lookupCity, obj1.name);
                comp2 = lvn(lookupCity, obj2.name);
                if (comp1 < comp2) {
                    return -1;
                } else if (comp1 > comp2) {
                    return 1;
                } else {
                    return 0;
                }
            }).slice(1, 11);;

            // Remove redundant information from the array
            top10Items.forEach(function (city, index) {
                top10Items[index] = {
                    name: [city.name, city.country, city.admin1].join(),
                    latitude: city.latitude,
                    longitude: city.longitude,
                    score: (10 - index)/10 
                };
            });
        }
        

        res.end(JSON.stringify({
            suggestions: top10Items
        }));
    } else {
        res.writeHead(404, { 'Content-Type': 'text/plain' });
        res.end();
    }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);