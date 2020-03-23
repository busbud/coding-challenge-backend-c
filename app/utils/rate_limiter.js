const {
  RateLimiterMemory,
  RateLimiterQueue
} = require("rate-limiter-flexible");
const RateLimiterQueueError = require("rate-limiter-flexible/lib/component/RateLimiterQueueError");

const rateLimiterMemory = new RateLimiterMemory({
  points: 30, // Limits 30 req/s. Still needs tunning.
  duration: 1, // Window of 1 second.
  blockDuration: 5 // Block of 5 seconds after limit reached.
});
const rateLimiter = new RateLimiterQueue(rateLimiterMemory, {
  maxQueueSize: 100 // Queues 100 requests before throwing 429 error.
});

const rateLimiterMiddleware = async (req, res, next) => {
  try {
    const remainingTokens = await rateLimiter.removeTokens(1);
    next();
  } catch (err) {
    if (err instanceof RateLimiterQueueError) {
      console.log("Out of tokens");
      res.status(429).send("Too Many Requests");
    } else {
      res.status(400).end();
    }
  }
};

module.exports = rateLimiterMiddleware;
