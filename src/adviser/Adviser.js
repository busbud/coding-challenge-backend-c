const assert = require('assert');
const { AdviserWriter } = require('./AdviserWriter');

class Adviser {
  constructor({ logger, config, score, connection }) {
    assert(logger, 'expected logger');
    assert(config, 'expected config');
    assert(connection, 'expected connection');

    this.logger = logger;
    this.config = config;
    this.score = score;
    this.connection = connection;
  }

  async getAdvice({ q, latitude, longitude }) {
    assert(q, 'expected q');
    // read cities
    const db = await this.connection.db(this.config.databases.database);
    const collection = db.collection(this.config.databases.collection);
    const cursor = await collection.find({ population: { $gte: this.config.suggestion.minPopulation } }, {}); // TODO filter to have one unique city name if no latitude and longitude
    const cityReader = cursor.stream();
    // pipe to writer
    const advertiserWriter = new AdviserWriter({
      config: this.config,
      logger: this.logger,
      score: this.score,
      userEntry: { q, latitude, longitude }
    });
    return new Promise((resolve, reject) => {
      advertiserWriter.on('finish', () => {
        this.logger.info('All cities processed');
        resolve(advertiserWriter.getResult());
      });
      [cityReader, advertiserWriter].forEach(emitter => {
        emitter.on('error', (err) => {
          this.logger.info('Something wrong happened while streamming');
          reject(err);
        });
      });
      cityReader.pipe(advertiserWriter);
    });
  }
}

module.exports = {
  Adviser,
};