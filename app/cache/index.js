const mcache = require("memory-cache");

const cache = (duration = 10) => {
  return (req, res, next) => {
    let key = "__express__" + req.originalUrl || req.url;
    let cachedResponse = mcache.get(key);
    if (cachedResponse) {
      res.send(cachedResponse);
      return;
    } else {
      res.sendResponse = res.send;
      res.send = (body) => {
        mcache.put(key, body, duration * 1000);
        res.sendResponse(body);
      };
      next();
    }
  };
};

module.exports = cache;
