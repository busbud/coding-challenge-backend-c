var geoip = require('geoip-lite');

module.exports.init = function (app) {

  function isLocalhost (ip, host){
    return ip === "127.0.0.1" || ip === "::ffff:127.0.0.1" || ip === "::1" || host.indexOf("localhost") !== -1;
  }

  /**
   * autodetect feature
   * if the autodetect param is set, then it will use geo-ip lite to
   * find the approximate location based on ip.
   */
  app.use((req, res, next) => {
    if (req.query.autodetect === '1' && !isLocalhost(req.ip, req.hostname)) {
      const geo = geoip.lookup(req.ip);

      req.query.latitude = geo.ll[0];
      req.query.longitude = geo.ll[1];
    }
    next();
  });
};