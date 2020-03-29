import http from 'http';
import url from 'url';
import dotenv from 'dotenv';
import { SearchCityFromFile, SearchCityFromEs } from './services';

dotenv.config();

const { SELECT_SERVICE = 'fileSearch', PORT } = process.env;
const port = PORT || 2345;

console.info('SELECT_SERVICE', SELECT_SERVICE);

const SEARCH_SERVICES = {
  elasticsearch: SearchCityFromEs,
  fileSearch: SearchCityFromFile
};

const searchService = SEARCH_SERVICES[SELECT_SERVICE].buildFromEnv();

function searchSuccess (cities, res) {
  if (cities.length <= 0) {
    searchFailure(res);
  } else {
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({
      suggestions: cities
    }));
  }
}

function searchFailure (res) {
  res.writeHead(404, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify({
    suggestions: []
  }));
}

module.exports = http.createServer(function (req, res) {
  // eslint-disable-next-line node/no-deprecated-api
  const urlParts = url.parse(req.url, { parseQueryString: true });
  if (urlParts.pathname === '/suggestions') {
    searchService.search(urlParts.query)
      .then((cities) => searchSuccess(cities, res))
      // eslint-disable-next-line handle-callback-err
      .catch(err => searchFailure(res));
  } else {
    res.end();
  }
}).listen(port, '0.0.0.0');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
