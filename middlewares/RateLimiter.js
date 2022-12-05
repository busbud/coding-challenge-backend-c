const rateLimit = require('express-rate-limit');

// this is a very simple rate limiter
// each user is limited to maxCalls (number of api calls) within the maxHours time period

const maxHours = 24;
const maxCalls = 100;

const RateLimiter = rateLimit({
  windowMs: maxHours * 60 * 60 * 1000, // convert hours to ms
  max: maxCalls,
  message: `You have exceeded the ${maxCalls} requests in ${maxHours} hrs limit!`, 
  standardHeaders: true,
  legacyHeaders: false,
});

module.exports = RateLimiter;