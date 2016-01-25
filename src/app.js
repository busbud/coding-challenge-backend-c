'use strict';

const port = process.env.PORT || 2345;

const http = require('http');
const url = require('url');
const querystring = require('querystring');
const LRU = require('lru-cache');

const tsv = require('./tsv');
const spatial = require('./spatial');
const util = require('./util');

const rowStream = tsv.fromFile('data/cities1000.tsv');
const res = spatial.fromStream(rowStream);
const tree = res.tree;
const trie = res.trie;

let cache = LRU({
  max: 500,
  dispose: function (key, n) { n = null; },
  maxAge: 1000 * 60 * 60
})

function calculateScore(leaf, latitude, longitude) {
  let distance = spatial.distance(leaf, {latitude, longitude});
  return 1 - (Math.log10(distance) / 10) || 0;
}

module.exports = http.createServer(function (req, res) {
  let prevResponse = cache.get(req.url);
  if (prevResponse) {
    res.writeHead(200, {'Content-Type': 'application/json; charset=utf-8'});
    res.end(prevResponse);
    return;
  }

  let parsedUrl = url.parse(req.url);
  if (parsedUrl.pathname === '/suggestions') {
    let query = querystring.parse(parsedUrl.query);
    const q = query.q;
    let latitude = parseFloat(query.latitude);
    let longitude = parseFloat(query.longitude);
    let radius = parseInt(query.radius);

    function err(message) {
      res.writeHead(400, {'Content-Type': 'application/json; charset=utf-8'});
      res.end(JSON.stringify({
        error: message
      }));
    }

    if (query.radius && radius <= 0) {
      err('Radius must be greater than 0 if specified');
      return;
    } else if (query.radius && radius == NaN) {
      err('Invalid radius');
      return;
    } else if (query.latitude && latitude == NaN) {
      err('Invalid latitude');
      return;
    } else if (query.longitude && longitude == NaN) {
      err('Invalid longitude');
      return;
    } else if (!query.longitude && query.latitude ||
               query.longitude && !query.latitude) {
      err('Latitude and longitude must both be specified');
      return;
    }

    if (q) {
      // filter duplicate suggestions
      // because the trie stores multiple possibilities of
      // each city.
      let suggestions = Array.from(new Set(trie.find(q)));
      for (var i in suggestions) {
        let original = suggestions[i];
        // Remove circular references and unnecessary keys
        suggestions[i] = util.clone(original);
        if(radius) {
          suggestions[i].nearby = tree.getNearby(original, radius, 10);
        }

        if(latitude && longitude) {
          suggestions[i].score = calculateScore(suggestions[i],
                                                latitude, longitude)
        }
      }
      suggestions.sort((a,b) => a.score - b.score);

      let response = util.stringify({suggestions});
      cache.set(req.url, response);
      res.writeHead(200, {'Content-Type': 'application/json; charset=utf-8'});
      res.end(response);

    } else {
      res.end(JSON.stringify({
        suggestions: []
      }));
    };
  } else {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end();
  }
}).listen(port);

console.log('Server running at %d', port);
