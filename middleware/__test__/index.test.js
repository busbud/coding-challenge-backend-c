const middleware = require('../');
const sinon = require('sinon');

describe('middleware', () => {
  describe('search geocoords by ip feature', () => {
    test('should find geoCoords when feature is enabled', () => {
      let ip = '142.243.136.34';
      const app = {
        middleware: null,
        use: function (fn) {
          this.middleware = fn;
        }
      };
      let req = {
        hostname: 'foo',
        ip: ip,
        query: {
          autodetect: '1'
        }
      };
      let res = {};
      let next = sinon.spy();

      middleware.init(app);

      app.middleware(req, res, next);

      expect(req.query.latitude).toEqual(45.4978);
      expect(req.query.longitude).toEqual(-73.5485);
      expect(next.called).toBeTruthy();
    });

    test('should _not_ find geoCoords when feature is disabled', () => {
      let ip = '142.243.136.34';
      const app = {
        middleware: null,
        use: function (fn) {
          this.middleware = fn;
        }
      };
      let req = {
        hostname: 'foo',
        ip: ip,
        query: {
          autodetect: '0'
        }
      };
      let res = {};
      let next = sinon.spy();

      middleware.init(app);

      app.middleware(req, res, next);

      expect(req.query.latitude).toBeUndefined();
      expect(req.query.longitude).toBeUndefined();
      expect(next.called).toBeTruthy();
    });

    test('should _not_ find geoCoords when running from localhost', () => {
      let ip = '127.0.0.1';
      const app = {
        middleware: null,
        use: function (fn) {
          this.middleware = fn;
        }
      };
      let req = {
        hostname: 'localhost',
        ip: ip,
        query: {
          autodetect: '1'
        }
      };
      let res = {};
      let next = sinon.spy();

      middleware.init(app);

      app.middleware(req, res, next);

      expect(req.query.latitude).toBeUndefined();
      expect(req.query.longitude).toBeUndefined();
      expect(next.called).toBeTruthy();
    });
  });
});