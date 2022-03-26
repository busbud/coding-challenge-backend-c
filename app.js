const http = require('http');
const { URLSearchParams } = require('url');
const { readCities } = require('./src/readCities');
const { buildSuggestions } = require('./src/suggestionGeneration');
const port = process.env.PORT || 2345;

const citiesFilePath = './data/cities_canada-usa.tsv';

// Only read the cities once and keep them in memory
const cities = readCities(citiesFilePath);

module.exports = http
    .createServer(function (req, res) {
        if (req.url.indexOf('/suggestions') === 0) {
            const queryString = new URLSearchParams(req.url.split('?')[1]);

            const searchInput = {
                searchText: queryString.get('q'),
                latitude: queryString.get('latitude'),
                longitude: queryString.get('longitude'),
            };

            if (searchInputIsInvalid(searchInput)) {
                res.writeHead(400);
                res.end();
                return;
            }

            const suggestions = buildSuggestions(cities, searchInput);

            const returnCode = suggestions.length > 0 ? 200 : 404;

            res.writeHead(returnCode, { 'Content-Type': 'application/json' });
            res.end(JSON.stringify({ suggestions: suggestions }));
        } else {
            res.writeHead(404, { 'Content-Type': 'text/plain' });
            res.end();
        }
    })
    .listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);

function searchInputIsInvalid(searchInput) {
    return (
        !searchInput.searchText ||
        (searchInput.latitude && !searchInput.longitude) ||
        (searchInput.longitude && !searchInput.latitude)
    );
}
