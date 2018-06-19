const express = require('express');

class Router {
  constructor({ logger, config, adviser }) {
    this.logger = logger;
    this.config = config;
    this.adviser = adviser;
    this.serviceDriver = this._initServer();
    this.registerRoutes();
  }

  start() {
    const port = process.env.PORT || 2345;
    this.serviceDriver.listen(port);

    console.log('Server running at http://127.0.0.1:%d/suggestions', port);
  }

  registerRoutes() {
    this.serviceDriver.get('/suggestions', (req, res) => this.suggestions(req, res));
    this.serviceDriver.get('*', (req, res) => res.status(404).end());
  }

  async suggestions(req, res) {
    const result = await this.adviser.getAdvice(req.query);
    res.status(200).send({ suggestions: result });
  }

  _initServer() {
    const serviceDriver = express();
    return serviceDriver;
  }
}

module.exports = {
  Router,
};