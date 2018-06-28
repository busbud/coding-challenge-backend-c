var RateLimit = require("express-rate-limit");

module.exports = app => {
  app.disable("x-powered-by");
  app.enable("trust proxy");

  const limiter = new RateLimit({
    windowMs: 5 * 60 * 1000,
    max: 120,
    delayMs: 300
  });

  app.use(limiter);
};
