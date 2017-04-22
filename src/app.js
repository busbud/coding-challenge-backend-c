/* eslint-disable no-console */
const fs = require('fs');
const http = require('http');
const url = require('url');

const coordinatePattern = require('./utils/regex').coordinatePattern;
const nameSearchBlacklist = require('./utils/regex').nameSearchBlacklist;
const getSuggestionsFromCache = require('./cache/api').getSuggestionsFromCache;
const setSuggestionsInCache = require('./cache/api').setSuggestionsInCache;
const getSuggestions = require('./suggestions/api').default;

const port = process.env.PORT || 80;
const ip = process.env.SERVER_IP || '127.0.0.1';

module.exports = http.createServer((req, res) => {
  function respondEmpty(response) {
    response.writeHead(404, { 'Content-Type': 'application/json' });
    response.end(JSON.stringify({ suggestions: [] }));
  }

  function respondSuggestions(response, suggestions) {
    response.writeHead(200, { 'Content-Type': 'application/json' });
    response.end(JSON.stringify({ suggestions }));
  }

  if (req.url.indexOf('/suggestions') === 0) {
    const queryParams = url.parse(req.url, true).query;

    // A bit of sanitization
    let nameQuery = queryParams.q || '';
    nameQuery = nameQuery.replace(nameSearchBlacklist, '');

    let lat = queryParams.latitude || null;
    lat = coordinatePattern.test(lat) ? lat : null;

    let lon = queryParams.longitude || null;
    lon = coordinatePattern.test(lon) ? lon : null;

    if (nameQuery === '' && (lat === null || lon === null)) {
      respondEmpty(res);
    }

    // Try the cache
    getSuggestionsFromCache(nameQuery, lat, lon).then((cachedSuggestions) => {
      if (cachedSuggestions !== null) {
        // Found in cache
        respondSuggestions(res, cachedSuggestions);
      } else {
        // Not in cache, do the search
        getSuggestions(nameQuery, lat, lon).then((suggestions) => {
          if (suggestions.length === 0) {
            respondEmpty(res);
          } else {
            respondSuggestions(res, suggestions);
          }
          // Put in cache
          setSuggestionsInCache(suggestions, nameQuery, lat, lon);
        });
      }
    });
  } else if (req.url.indexOf('/loaderio-803799113c7bc4393b267aefad51fb37.txt') === 0) {
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    fs.createReadStream('./src/files/loaderio.txt').pipe(res);
  } else {
    res.writeHead(404);
    res.end();
  }
}).listen(port, ip);

console.log(`Server running at http://${ip}:${port}/suggestions`);
