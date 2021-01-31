const prometheus = require('prom-client');

class ActuatorController {
    // eslint-disable-next-line no-unused-vars
    static async health(request, response, next) {
        return response.json({ health: 'UP' });
    }

    static async prometheus(request, response) {
        response.set('Content-Type', prometheus.register.contentType);
        response.send(await prometheus.register.metrics());
    }
}

module.exports = ActuatorController;
