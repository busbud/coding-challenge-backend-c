const {
  RateLimiterMemory,
  RateLimiterQueue
} = require("rate-limiter-flexible");
const RateLimiterQueueError = require("rate-limiter-flexible/lib/component/RateLimiterQueueError");

var setup = require("../properties");

const rateLimiterMemory = new RateLimiterMemory(setup.app.rateLimiter);
const rateLimiter = new RateLimiterQueue(
  rateLimiterMemory,
  setup.app.rateLimiterQueue
);

const rateLimiterMiddleware = async (req, res, next) => {
  try {
    const remainingTokens = await rateLimiter.removeTokens(1);
    console.log(remainingTokens);
    next();
  } catch (err) {
    if (err instanceof RateLimiterQueueError) {
      console.log("Out of tokens");
      res.status(429).send("Too Many Requests");
    } else {
      console.log("Plain error");
      res.status(400).end();
    }
  }
};

module.exports = rateLimiterMiddleware;
