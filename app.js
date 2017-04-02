var express = require('express');
var app = express();
exports.app = app;
var request = require('request');

app.set('port', process.env.PORT || 2345);

app.get('/suggestions', function (req, res) {
    
    // get the GET paramters
    var q = req.query.q;
    var latitude = req.query.latitude;
    var longitude = req.query.longitude;
    
    // check if they are defined and of the correct type
    if(q === undefined) {
        res.status(400).send(JSON.stringify({'error' : 'q is not defined' }));
        return;
    }
    if(latitude !== undefined && isNaN(latitude)) {
        res.status(400).send(JSON.stringify({'error' : 'latitude is not a number' }));
        return;
    }
    if(longitude !== undefined && isNaN(longitude)) {
        res.status(400).send(JSON.stringify({'error' : 'longitude is not a number' }));
        return;
    }
    
    var suggestions = [];
    
    // use the geonames JSON api
    // search for locations beginning with q param
    // filter cities with 5000 or more population
    // filter for Canada and United States
    // use eoincos as username
    request('http://api.geonames.org/searchJSON?name_startsWith=' + q + '&cities=cities5000&country=CA&country=US&username=eoincos', function (error, response, body) {
        if (!error && response.statusCode == 200) {
            var geonamesResponse = JSON.parse(body);
            var geonamesList = geonamesResponse.geonames;
            
            for (var key in geonamesList) {
                if (geonamesList.hasOwnProperty(key)) {
                    var currentLocation = geonamesList[key];
                    
                    // generate the name
                    // originally used adminCode1 instead of adminName1
                    // however this did not give the correct 2 letter province code for Canadian cities
                    var locationName = currentLocation.toponymName + ', ' + currentLocation.adminName1 + ', ' + currentLocation.countryCode;
                    
                    var locationLat = currentLocation.lat;
                    var locationLng = currentLocation.lng;
                    
                    //make latitude and longitude optional
                    if(latitude !== undefined && longitude !== undefined) {
                        var distance = getDistanceFromLatLonInKm(latitude, longitude, locationLat, locationLng);
                        
                        // if it is further than 800km away then remove it from the results
                        if(distance < 800) {
                            // the score is based on the distance to the latitude-longitude point
                            var score = (1 - (distance / 800)).toFixed(1);
                            
                            suggestions.push({
                                name: locationName,
                                latitude: locationLat,
                                longitude: locationLng,
                                score: score
                            });
                        }
                    } else {
                        suggestions.push({
                            name: locationName,
                            latitude: locationLat,
                            longitude: locationLng
                        });
                    }
                }
            }
        }
        
        // sort based on highest score
        if(latitude !== undefined && longitude !== undefined) {
            suggestions.sort(function(a, b){return b.score-a.score});
        }
        
        if(suggestions.length > 0) {
            res.send(JSON.stringify({'suggestions' : suggestions }));
        }
        else {
            //if there are no suggestions then return a 404
            res.status(404).send(JSON.stringify({'suggestions' : suggestions }));
        }
    });
})

app.listen(app.get('port'),
    function(){
        console.log("Server listening on port " + app.get('port'));
    }
)

// the Haversine formula for calculating the distance between two latitude-longitude points
// taken from stack overflow: http://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula
function getDistanceFromLatLonInKm(lat1,lon1,lat2,lon2) {
  var R = 6371; // Radius of the earth in km
  var dLat = deg2rad(lat2-lat1);  // deg2rad below
  var dLon = deg2rad(lon2-lon1); 
  var a = 
    Math.sin(dLat/2) * Math.sin(dLat/2) +
    Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) * 
    Math.sin(dLon/2) * Math.sin(dLon/2)
    ; 
  var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a)); 
  var d = R * c; // Distance in km
  return d;
}

function deg2rad(deg) {
  return deg * (Math.PI/180)
}