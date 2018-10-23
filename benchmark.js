"use strict";
var apiBenchmark = require('api-benchmark');
const path = require('path');
const fs = require("fs");

function getRandomCity() {
    var text = "";
    var possible = "abcdefghijklmnopqrstuvwxyz";

    for (var i = 0; i < 3; i++)
        text += possible.charAt(Math.floor(Math.random() * possible.length));

    return text;
}

function getRandomLatitude() {
    let max = 42.0126;
    let min = 32.5121;
    return Math.random() * (max - min) + min;
}

function getRandomLongitude() {
    let max = -114.1315;
    let min = -124.6509;
    return Math.random() * (max - min) + min;
}

var service = {
    server1: 'http://127.0.0.1:2345/'
};

var routes = { route1: {
    method: 'get',
    route: 'suggestions',
    query: function() {
        return {
            q: getRandomCity(),
            latitude: getRandomLatitude(),
            longitude: getRandomLongitude()
        };
    }
}
};

apiBenchmark.measure(service, routes, function(err, results) {
    console.log(JSON.stringify(results,null,2));
    apiBenchmark.getHtml(results, function(error,html){
        if (html) {
            fs.writeFileSync(path.join(__dirname,'/benchmark.html'), html);
        }

    });
    // displays some stats!
});