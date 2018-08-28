const {expect}  = require('chai');
const config = require('config');
const mock = require('mock-require');

let logger;

function decache(module) {
  delete require.cache[require.resolve(module)];
}

describe('lib/logger', () => {
  describe('with a production env value', () => {
    before('stubs', () => {
      decache('./logger');
      decache('config');
      mock('config', Object.assign({}, {...config, env: 'production'}));
    });

    before('load logger', () => {
      logger = require('./logger')();
    });

    it('should have initialized without error', () => {
      expect(logger).to.be.an('object');
    });

    after('un-stub', () => {
      mock.stop('config');
      decache('config');
    });

    after('decache logger', () => {
      decache('./logger');
    });
  });
});
