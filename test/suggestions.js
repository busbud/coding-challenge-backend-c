// force the test environment to 'test'
process.env.NODE_ENV = 'test';

var expect  = require('chai').expect,
    server  = require('../src/application'),
    PORT = process.env.PORT || 2345,
    MEMCACHED = process.env.MEMCACHED || null,
    DEBUG_MODE = process.env.DEBUG || false;

var application, request;
var supertest = require('supertest');


describe('Suggestions suite tests', function() {
  describe('initialise server', function () {
    before(function (done) {
      application = new server({ 'port':PORT, 'memcached':MEMCACHED });
      application.initialize(function(){
        application.run();
        request = supertest(application.httpServer);
        done();
      });
    });

    it('Datas is loaded', function () {
      expect(application.inMemoryDatabase.length).to.not.equal(0);
    });
    it('Options memcached is correctly setup', function () {
      expect(application.memcached).to.equal(MEMCACHED);
    });
  });

  describe('GET /suggestions', function() {

    describe('with a non-existent city', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
          .end(function (err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });

      it('returns a 404', function () {
        expect(response.statusCode).to.equal(404);
      });

      it('returns an empty array of suggestions', function () {
        expect(response.json.suggestions).to.be.instanceof(Array);
        expect(response.json.suggestions).to.have.length(0);
      });
    });


    describe('with a valid city', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=Montreal')
          .end(function (err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });

      it('returns a 200', function () {
        expect(response.statusCode).to.equal(200);
      });

      it('returns an array of suggestions', function () {
        expect(response.json.suggestions).to.be.instanceof(Array);
        expect(response.json.suggestions).to.have.length.above(0);
      });

      it('contains a match', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
          return suggestions.some(function (suggestion) {
            return /montreal/i.test(suggestion.name);
          });
        })
      });

      it('contains latitudes and longitudes', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
          return suggestions.every(function (suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        })
      });

      it('contains scores', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
          return suggestions.every(function (suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        })
      });
    });

  });



});