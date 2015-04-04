var expect = require('chai').expect;
var app = require('../../app');
var request = require('supertest')(app);

describe('GET /mileage', function() {

  describe('with a missing param', function() {
    var response;

    before(function(done) {

      request
        .get('/mileage?departure_latitude=40.46423&arrival_latitude=45.50884&arrival_longitude=-73.58781')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400', function() {
      expect(response.statusCode).to.equal(400);
    });

  });

  describe('with valid params', function() {
    var response;

    before(function(done) {
      request
        .get('/mileage?departure_latitude=40.46423&departure_longitude=-80.60091&arrival_latitude=45.50884&arrival_longitude=-73.58781')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', function() {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an object containing mileage in kms and miles', function() {
      expect(response.json.mileage).to.be.instanceof(Object);
      expect(response.json.mileage).to.have.property('kms');
      expect(response.json.mileage).to.have.property('miles');
    });

  });
});
