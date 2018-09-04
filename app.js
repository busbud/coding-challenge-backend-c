//first node.js project 

var http = require('http');
var url = require('url');
var fs = require('fs');

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

        res.end(JSON.stringify({
            suggestions: objContainer
        }));
    } else {
        res.writeHead(404, { 'Content-Type': 'text/plain' });
        res.end();
    }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);