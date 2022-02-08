var http = require('http');
var port = process.env.PORT || 2345;
const url = require('url');
const fs = require('fs');
const QuickScore = require("quick-score").QuickScore;

const citiesData = './data/cities_canada-usa.tsv';

module.exports = http.createServer(function (req, res) {
    res.writeHead(404, {'Content-Type': 'text/plain'});

    if (req.url.indexOf('/suggestions') === 0) {
        const params = url.parse(req.url, true).query;
        const query = params.q;

        if (!query) {
            res.end(JSON.stringify({
                suggestions: []
            }));
        }

        const options = optionsFromParams(params);

        fs.readFile(citiesData, "utf8", function (err, data) {
            if (err) throw err;

            const json = jsonFromTsv(data);
            const score = new QuickScore(json, options);
            const results = score.search(query);

            res.writeHead(200, {'Content-Type': 'text/plain'});
            res.end(JSON.stringify({
                suggestions: results.map((record) => {
                    return {
                        name: record.item.name,
                        latitude: record.item.lat,
                        longitude: record.item.long,
                        score: record.score,
                    }
                })
            }));
        });
    } else {
        res.end();
    }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);


function jsonFromTsv(data) {
    let rows = data.split("\n");
    let result = [];
    let columns = rows[0].split("\t");

    for (let i = 1; i < rows.length; i++) {
        let row = {};
        let currentline = rows[i].split("\t");

        for (let [key, column] of columns.entries()) {
            if ((['name', 'lat', 'long'].includes(column))) {
                row[column] = currentline[key];
            }
        }

        result.push(row);
    }

    return result;
}

function optionsFromParams(params) {
    let options = ['name'];

    const lat = params.latitude;
    const long = params.longitude;

    if (lat && long) {
        options.push('lat');
        options.push('long');
    }

    return options;
}