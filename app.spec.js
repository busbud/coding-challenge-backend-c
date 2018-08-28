const {expect}  = require('chai');
const config = require('config');
const mock = require('mock-require');
const {stub} = require('sinon');
let app;

function decache(module) {
  delete require.cache[require.resolve(module)];
}

describe('app init', () => {
  describe('with an invalid env value', () => {
    before('stubs', () => {
      decache('./app');
      decache('config');
      mock('config', Object.assign({}, {...config, env: 'unsupportedEnv'}));
      stub(process, 'exit');
    });

    before('load express and wait until it\'s ready', () => {
      app = require('./app');
    });

    it('should have called process.exit', () => {
      expect(process.exit.callCount).to.equal(1);
    });

    after('un-stub', () => {
      mock.stop('config');
      decache('config');
      process.exit.restore();
    });

    after('decache express server', () => {
      decache('./app');
    });
  });

  describe('with a production env value', () => {
    before('stubs', () => {
      decache('./app');
      decache('config');
      mock('config', Object.assign({}, {...config, env: 'production', port: 2346}));
      stub(process, 'exit');
    });

    before('load express and wait until it\'s ready', () => {
      app = require('./app');
    });

    it('should have initialized without error', () => {
      expect(process.exit.callCount).to.equal(0);
    });

    it('should fire the serverReady event', done => {
      app.on('serverReady', done);
    });

    after('un-stub', () => {
      mock.stop('config');
      decache('config');
      process.exit.restore();
    });

    after('decache express server', () => {
      decache('./app');
    });
  });
});
