const config = require('config');
const logger = require('logger');
const { MongoClient } = require('mongodb');
const { Adviser, Score } = require('./adviser');
const { Router } = require('./router');

class Main {
  async init() {
    this.config = config;
    this.connection = await this.initConnection();
    this.logger = logger.createLogger();

    this.initInstances();

    this.router = new Router(this);
  }

  start() {
    this.router.start();
  }

  initInstances() {
    this.score = new Score(this);
    this.adviser = new Adviser(this);
  }

  async initConnection() {
    const connection = await MongoClient.connect(config.databases.url, { useNewUrlParser: true });
    return connection;
  }
}



module.exports = {
  Main,
};
