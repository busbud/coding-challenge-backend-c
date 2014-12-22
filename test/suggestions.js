
/**
 * Module dependencies.
 */

var expect = require('chai').expect;
var supertest = require('supertest');

// This is to influence the server so that it may return up to 5 results.
process.env.MAX_RESULTS = 5;
var app = require('../app');


/**
 * Jslint global directives.
 */

/*global describe, before, it */
/*jslint regexp: true*/


/**
 * Module variables.
 */

var request;


describe('validating server routes', function () {

/**
 * General interface tests.
 */

  describe('with an incorrect path', function () {
    var response;

    before(function (done) {
      app.initServer('./data/cities_canada-usa.tsv', function (err, server) {
        expect(err).to.be.null();
        expect(server).to.not.be.undefined();
        request = supertest(server);
        request
          .get('/bad')
          .end(function (err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });
    });

    it('returns a 404', function () {
      expect(response.statusCode).to.equal(404);
    });

    it('returns the correct information', function () {
      expect(response.json.status).to.equal('404');
      expect(response.json.code).to.equal('InvalidPath');
      expect(response.json.message).to.equal('Invalid path.');
      expect(response.json).to.have.property('more info');
    });
  });


/**
 * Interface specific tests.
 */

  describe('GET /suggestions', function () {
    describe('with a missing query parameter', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions')
          .end(function (err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });
      it('returns a 400', function () {
        expect(response.statusCode).to.equal(400);
      });

      it('returns the correct information', function () {
        expect(response.json.status).to.equal('400');
        expect(response.json.code).to.equal('MissingRequiredQueryParameter');
        expect(response.json.message).to.equal('A required query parameter was not specified for this request.');
        expect(response.json).to.have.property('more info');
      });
    });

    describe('with an undefined query parameter', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=')
          .end(function (err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });
      it('returns a 400', function () {
        expect(response.statusCode).to.equal(400);
      });

      it('returns the correct information', function () {
        expect(response.json.status).to.equal('400');
        expect(response.json.code).to.equal('MissingRequiredQueryParameter');
        expect(response.json.message).to.equal('A required query parameter was not specified for this request.');
        expect(response.json).to.have.property('more info');
      });
    });

    describe('with an incorrect query parameter', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=foobar&latitude=bad')
          .end(function (err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });
      it('returns a 400', function () {
        expect(response.statusCode).to.equal(400);
      });

      it('returns the correct information', function () {
        expect(response.json.status).to.equal('400');
        expect(response.json.code).to.equal('InvalidQueryParameterValue');
        expect(response.json.message).to.equal('An invalid value was specified for one of the query parameters in the request URI.');
        expect(response.json).to.have.property('more info');
      });
    });

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
          return suggestions.every(function (suggestion) {
            return (/^montreal.*/i).test(suggestion.name);
          });
        });
      });

      it('contains latitudes and longitudes', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
          return suggestions.every(function (suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        });
      });

      it('contains scores', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
          return suggestions.every(function (suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        });
      });

      it('matches known results', function () {
        expect(response.json.suggestions).to.have.length(2);

        expect(response.json.suggestions[0].name).to.equal('Montreal, QC, Canada');
        expect(response.json.suggestions[0].score).to.equal(1.0);
        expect(response.json.suggestions[1].name).to.equal('Montreal-Ouest, QC, Canada');
        expect(response.json.suggestions[1].score).to.equal(0.7);
      });
    });

    describe('with a valid city and coordinates', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163')
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
          return suggestions.every(function (suggestion) {
            return (/londo/i).test(suggestion.name);
          });
        });
      });

      it('contains latitudes and longitudes', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
          return suggestions.every(function (suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        });
      });

      it('contains scores', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
          return suggestions.every(function (suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        });
      });

      it('matches known results', function () {
        expect(response.json.suggestions).to.have.length(5);

        expect(response.json.suggestions[0].name).to.equal('London, ON, Canada');
        expect(response.json.suggestions[0].score).to.equal(0.9);
        expect(response.json.suggestions[1].name).to.equal('London, OH, USA');
        expect(response.json.suggestions[1].score).to.equal(0.5);
        expect(response.json.suggestions[2].name).to.equal('London, KY, USA');
        expect(response.json.suggestions[2].score).to.equal(0.5);
        expect(response.json.suggestions[3].name).to.equal('Londontowne, MD, USA');
        expect(response.json.suggestions[3].score).to.equal(0.3);
        expect(response.json.suggestions[4].name).to.equal('Londonderry, NH, USA');
        expect(response.json.suggestions[4].score).to.equal(0.3);
      });
    });
  });
});