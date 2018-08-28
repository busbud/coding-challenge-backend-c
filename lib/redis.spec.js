const {expect}  = require('chai');
const mock = require('mock-require');
const {stub} = require('sinon');
const config = require('config');
let redis;

function decache(module) {
  delete require.cache[require.resolve(module)];
}

describe('lib/redis', () => {
  describe('when pool fails to initialize', () => {
    before('stubs', () => {
      decache('./redis');
      decache('sol-redis-pool');
      mock('sol-redis-pool', () => {
        throw new Error();
      });
      stub(process, 'exit');
    });

    before('load redis', () => {
      redis = require('./redis');
    });

    it('should have called process.exit', () => {
      expect(process.exit.callCount).to.equal(1);
    });

    after('un-stub', () => {
      mock.stop('sol-redis-pool');
      decache('sol-redis-pool');
      process.exit.restore();
    });

    after('decache logger', () => {
      decache('./redis');
    });
  });

  describe('when pool properly initializes', () => {
    before('stubs', () => {
      decache('./redis');
    });

    before('load redis', () => {
      redis = require('./redis');
    });

    it('should have called process.exit', () => {
      expect(redis).to.be.a('function');
    });

    after('decache logger', () => {
      decache('./redis');
    });
  });

  describe('when using a redis url', () => {
    before('stubs', () => {
      decache('./redis');
      decache('sol-redis-pool');
      decache('config');
      mock('config', Object.assign({}, {...config, db: {redis: {...config.db.redis, url: 'redis://localhost:6379/1'}}}));
    });

    before('load redis', () => {
      redis = require('./redis');
    });

    it('should have called process.exit', () => {
      expect(redis).to.be.a('function');
    });

    after('un-stub', () => {
      mock.stop('sol-redis-pool');
      mock.stop('config');
      decache('sol-redis-pool');
      decache('config');
    });

    after('decache logger', () => {
      decache('./redis');
    });
  });
});
