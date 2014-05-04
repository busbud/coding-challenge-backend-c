var http = require('http');

var url = require('url');
var fs = require('fs');
var tsv = require('tsv');

var parseQuery = function(requestUrl) {
    parsedUrl = url.parse(requestUrl, true);
    return parsedUrl.query;
}

var readTable = function() {
    var tableText = fs.readFileSync('data/cities_canada-usa.tsv','utf-8');
    return tsv.parse(tableText);
}

var lookup = function(query) {
    var q = query.q.toLowerCase();
    var res = [];
    table.forEach(function(element,index) {
        if(element.ascii != undefined &&
            element.ascii.toLowerCase().indexOf(q) != -1) {
            /*var city = {
                name: element.name,
                ascii: element.ascii,
                lat: element.lat,
                long: element.long
            }*/
            res.push(element);
        }
    });
    return res;
}

var score = function(cities,query) {
    var res = [];
    cities.forEach(function(element) {
        var len = element.ascii.length;
        var qLen = query.q.length;
        var score = qLen/len;

        var long = (query.long || query.longitude);
        if(long)
        {
            var longScore = (360 - Math.abs(element.long - long)) / 360;
            score += longScore;
            score /= 2;
        }

        var lat = (query.lat || query.latitude);
        if(lat)
        {
            var latScore = (180 - Math.abs(element.lat - lat)) / 180;
            score += latScore;
            score /= 2;
        }

        element.score = score;

        var city = {};
        city.name = element.name;
        city.score = score;
        city.long = element.long;
        city.lat = element.lat;

        res.push(city);
    });

    //return cities;
    return res;
}

var port = process.env.PORT || 2345;

var table = readTable();

/*var result = lookup("new y");

console.log(JSON.stringify(result));*/

var server = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
    res.writeHead(200, {'Content-Type': 'application/json; charset=utf-8'});
    var query = parseQuery(req.url);
    var result = {};
    result.suggestions = [];
    if(query.q != undefined)
    {
        var cities = lookup(query);
        result.suggestions = score(cities,query);
    }
    //console.log(suggestions);
    res.end(JSON.stringify(result,null," "));
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
