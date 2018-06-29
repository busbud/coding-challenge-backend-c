var RateLimit = require("express-rate-limit");
module.exports = app => {
  app.enable("trust proxy");

  var limiter = new RateLimit({
    windowMs: 5 * 60 * 1000,
    max: 120,
    delayMs: 300
  });

  app.use(limiter);
};
