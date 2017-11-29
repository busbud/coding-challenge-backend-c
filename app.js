var http = require('http');
var port = process.env.PORT || 2345;
var X_TOKEN = "PARTNER_JSWsVZQcS_KzxNRzGtIt1A";

var toobusy = require('toobusy-js'),
    express = require('express');

var fs = require("fs");
var es = require("event-stream");
var vsprintf = require("sprintf").vsprintf;
var util = require("util");
var Tree = require("./tree.js");

var tree = new Tree();
// Read File
fs.createReadStream("data/cities_canada-usa.tsv")
    // Split Strings
    .pipe(es.split("\n"))
    // Split Strings into Array
    .pipe(es.mapSync(function(data) {
        var datas = data.split("\t");
        var cityName = datas[2];
        var countryCode = datas[8];
        var cc2 = datas[10];
        var country;
        switch(countryCode) {
            case "CA" : country = "Canada"; break;
            case "US" : country = "USA"; break;
        }
        var latitude = datas[4];
        var longitude = datas[5];
        var population = datas[14];
        var name = cityName + ", " + cc2 + ", " + country;
        if(cityName !== undefined && population > 5000) {
            var now = Math.round(Date.now() /1000);
            var position = {"latitude" : latitude, "longitude" : longitude};
            tree.add(cityName, {name: name, position: position, count: 0, distance: -1});
        }
}));

var app = express();

// middleware which blocks requests when we're too busy
app.use(function(req, res, next) {
    if (toobusy()) {
        res.send(503, "I'm busy right now, sorry.");
    } else {
        next();
    }
});

// POST suggestions
app.post('/suggestions', function(req, res) {
    var q = req.query.q;
    var latitude = req.query.latitude;
    var longitude = req.query.longitude;

    var contentType = req.headers["content-type"];
    var xToken = req.headers["x-busbud-token"];
    if(q === undefined) {
        res.status(404).end("Oups, the query is not correct !");
    } else {
        if(contentType !== "application/json") {
            res.status(500).end("Oups, the content type is not correct !");
        } else {
            if(xToken !== X_TOKEN) {
                res.status(500).end("Oups, the token is not correct");
            } else {
                var nodes = tree.search(q, latitude, longitude);
                res.writeHead(200, {'Content-Type': 'application/json'});
                res.end(JSON.stringify({
                    suggestions: nodes
                }));
            }
        }
    }
});

var server = app.listen(process.env.PORT || 2345)

// kill the server
process.on('SIGINT', function() {
    server.close();
    // calling .shutdown allows your process to exit normally
    toobusy.shutdown();
    process.exit();
});

console.log('Server running at http://127.0.0.1:%d/suggestions', port);

module.exports = app;