var mcache = require("memory-cache");

module.exports = function(
  duration,
  keyFunc = req => req.originalUrl || req.url
) {
  return (req, res, next) => {
    var key = keyFunc(req);
    let cachedBody = mcache.get(key);
    if (cachedBody) {
      res.send(cachedBody);
      return;
    }
    res.sendResponse = res.send;
    res.send = body => {
      mcache.put(key, body, duration * 1000);
      res.sendResponse(body);
    };
    next();
  };
};
