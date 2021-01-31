const prometheus = require('prom-client');

prometheus.collectDefaultMetrics();

const requestCounter = new prometheus.Counter({
    name: 'suggestion_service:request_count',
    help: 'Counter for requests on the suggestions API',
    labelNames: ['uri'],
});

const countRequests = (req, res, next) => {
    requestCounter.inc({ uri: req.path });
    next();
};

module.exports = {
    countRequests,
};
