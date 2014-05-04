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
    var q = query.toLowerCase();
    console.log(q)
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

var port = process.env.PORT || 2345;

var table = readTable();

/*var result = lookup("new y");

console.log(JSON.stringify(result));*/

var server = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
    res.writeHead(200, {'Content-Type': 'text/plain'});
    var query = parseQuery(req.url);
    var suggestions = [];
    if(query.q != undefined)
    {
        suggestions = lookup(query.q);
    }
    console.log(suggestions);
    res.end(JSON.stringify(suggestions));
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
