const CircuitBreaker = require('opossum');
const handleRequest = require('../lib/handleRequest');

const circuitBreakerOptions = {
  timeout: 3000, // If our function takes longer than 3 seconds, trigger a failure
  errorThresholdPercentage: 100, // When 100% of requests fail, trip the circuit
  resetTimeout: 5000, // After 5 seconds, try again.
};

const breaker = new CircuitBreaker(
  handleRequest.getFromCacheOrProcess,
  circuitBreakerOptions,
);

const sendResponse = (req, res, result) => {
  res.set('Cache-Control', 'public, max-age=31557600, s-maxage=31557600'); // 1 year
  if(result.time) {
    // This is not proper http for this case...
    // If data was generated, this would indicate generation date.
    // Not so valid for cities which are constant.
    res.set('Last-Modified', new Date(result.time).toUTCString());
  }
  if (!result || !result.data || !result.data.length) {
    return res.status(404).json({
      suggestions: [],
    });
  }
  res.json({
    suggestions: result.data,
  });
};

exports.handleSuggestions = async (req, res) => {
  try {
    const result = await breaker.fire(req, res);
    return sendResponse(req, res, result);
  } catch (err) {
    // send the error to the end-user? challenge purposes only, don't do this in prod!
    return sendResponse(req, res, err);
  }
};