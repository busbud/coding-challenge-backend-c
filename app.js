const http = require('http');
const port = process.env.PORT || 2345;
const url = require('url');
const fs = require('fs');
const QuickScore = require("quick-score").QuickScore;
const NodeCache = require("node-cache");
const {jsonFromTsv, optionsFromParams} = require('./utils/helpers');
const cache = new NodeCache();

const citiesData = './data/cities_canada-usa.tsv';

module.exports = http.createServer((req, res) => {
    res.writeHead(404, {'Content-Type': 'text/plain'});

    if (req.url.indexOf('/suggestions') === 0) {
        const params = url.parse(req.url, true).query;
        const query = params.q;

        if (!query) {
            return res.end(JSON.stringify({suggestions: []}));
        }

        const key = JSON.stringify(params);
        const suggestions = cache.get(key);

        if (suggestions) {
            res.writeHead(200, {'Content-Type': 'text/plain; charset=utf-8'});
            return res.end(JSON.stringify({suggestions}));
        }

        const options = optionsFromParams(params);

        fs.readFile(citiesData, "utf8", (err, data) => {
            if (err) throw err;

            const json = jsonFromTsv(data);
            const score = new QuickScore(json, options);
            const results = score.search(query);

            if (!results.length) {
                return res.end(JSON.stringify({suggestions: []}));
            }

            const suggestions = results.map((record) => {
                return {
                    name: record.item.ascii,
                    latitude: record.item.lat,
                    longitude: record.item.long,
                    score: record.score,
                }
            });

            cache.set(key, suggestions);

            res.writeHead(200, {'Content-Type': 'text/plain; charset=utf-8'});
            return res.end(JSON.stringify({suggestions}));
        });
    } else {
        res.end();
    }
}).listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
