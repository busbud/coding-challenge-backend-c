const http = require('http');
const url = require('url');
const port = process.env.PORT || 2345;
const getCityDataAndScoreFromGeoDataModule = require('./getCityDataAndScoreFromGeoData.js');

/**
 * Validates client request parameters.
 * @param {string} Request path of the client request.
 * @param {Object} URLSearch object to validate the URL.
 * @return {string} Returns the error message if client request doesn't validate
 * Else returns an empty string indicting success.
 */
function validateClientRequestAndGetErrors(requestPath, urlSearch) {
  if (requestPath !== '/suggestions') {
    return `Request path: ${requestPath} invalid.`;
  }

  if (!urlSearch.has('q')) {
    return 'Request missing parameter q.';
  }

  if (!urlSearch.get('q') && urlSearch.get('q').length === 0) {
    return 'Request contains invalid value for q.';
  }

  if (!urlSearch.has('latitude') && urlSearch.has('longitude')) {
    return 'Request contains parameter longitude but is missing latitude.';
  }

  if (!urlSearch.has('longitude') && urlSearch.has('latitude')) {
    return 'Request contains parameter latitude but is missing longitude.';
  }

  if (urlSearch.has('latitude') && urlSearch.has('longitude')) {
    const lat = parseFloat(urlSearch.get('latitude'));
    const longitude = parseFloat(urlSearch.get('longitude'));
    if (Number.isNaN(lat) || Number.isNaN(longitude)) {
      return 'Request contains invalid values for latitude/longitude.';
    }
  }

  return '';
}

/**
 * Request handler for client connections.
 * @param {object} Client Request.
 * @param {object} Response to be returned to the client.
 */
const requestHandler = (request, response) => {
  const req = url.parse(request.url, true);

  const urlSearch = new URLSearchParams(req.search);
  const errorsDuringValidation = validateClientRequestAndGetErrors(req.pathname, urlSearch);
  if (errorsDuringValidation.length === 0) {
    const region = urlSearch.get('q');

    // During validation we check either both latitude and longitude are present or neither.
    let longitude = null;
    let latitude = null;
    if (urlSearch.has('latitude') && urlSearch.has('longitude')) {
      latitude = parseFloat(urlSearch.get('latitude'));
      longitude = parseFloat(urlSearch.get('longitude'));
    }

    getCityDataAndScoreFromGeoDataModule.getCityDataWithScore(region, latitude, longitude,
      (err, responseJson, responseStatusCode) => {
        if (!err) {
          response.statusCode = responseStatusCode;
          response.write(responseJson);
          response.end();
        } else {
          response.statusCode = responseStatusCode;
          response.end(err.message);
        }
      });
  } else {
    response.statusCode = 400;
    // console.log('Bad request: ' + req.toString() + ' error: ' + errorsDuringValidation);
    response.write(errorsDuringValidation);
    response.end();
  }
};

module.exports = http.createServer(requestHandler).listen(port, '0.0.0.0');
