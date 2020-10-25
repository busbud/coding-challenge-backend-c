const express = require('express');

const app = express();

const defaultport = process.env.PORT || 2345;

const config = require('config');

const RateLimit = require('express-rate-limit');
const CircuitBreaker = require('opossum');
const handleRequest = require('./lib/handleRequest');
const data = require('./lib/dataParser');

const port = config.port ? config.port : defaultport;

app.set('trust proxy', '127.0.0.1');

const limiter = new RateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // limit each IP to 100 requests per windowMs
  delayMs: 0,
});

// apply rate-limiting to all requests
app.use(limiter);

// load cities on startup. on error, fail hard.
data.loadData(config.datafile, handleRequest.trie, config)
  .catch((err) => { console.error('err'); console.error(err); process.exit(); });

app.listen(port, () => {
  console.log(`App listening on port : ${port}`);
});

app.get('/', (req, res) => {
  res.send('Welcome! This site only has one API endpoint implemented. See /suggestions/?q=Londo');
});

const circuitBreakerOptions = {
  timeout: 3000, // If our function takes longer than 3 seconds, trigger a failure
  errorThresholdPercentage: 100, // When 100% of requests fail, trip the circuit
  resetTimeout: 5000, // After 5 seconds, try again.
};
const breaker = new CircuitBreaker(handleRequest.getFromCacheOrProcess, circuitBreakerOptions);

const sendResponse = async (req, response, result) => {
  response.set('Cache-Control', 'public, max-age=31557600, s-maxage=31557600'); // 1 year
  response.status(result && result.data && result.data.length > 0 ? 200 : 404);
  if (result.time != null) {
    // This is not proper http for this case...
    // If data was generated, this would indicate generation date.
    // Not so valid for cities which are constant.
    response.set('Last-Modified', new Date(result.time).toUTCString());
  }
  response.send(JSON.stringify({
    suggestions: result.data || [],
  }));
  Promise.resolve();
};

function sendToBreaker(req, res) {
  return breaker.fire(req, res)
    .then((result) => sendResponse(req, res, result))
    .catch((err) => sendResponse(req, res, err));
  // send the error to the end-user? challenge purposes only, don't do this in prod!
}

app.get('/suggestions', sendToBreaker);

module.exports = app;
