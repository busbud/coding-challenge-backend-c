const mcache = require('memory-cache');

const cache = (duration, formatKey = (req) => req.originalUrl || req.url) =>
    (req, res, next) => {
        const key = formatKey(req);
        const value = mcache.get(key);
        if (value) {
            res.send(JSON.parse(value));
            return;
        }

        res.sendResponse = res.send;
        res.send = (body) => {
            mcache.put(key, body, duration * 1000);
            res.sendResponse(body);
        };

        next();
    };

module.exports = cache;
