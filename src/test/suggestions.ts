/* eslint-disable */
import { expect } from 'chai';
import routes from '../app/routes';
import HttpServer from '../infra/http/http-server';
import { IOrmClient } from '../infra/orm/orm-client-interface';
import { SequelizeClient } from '../infra/orm/sequelize-client';
import supertest, { Response } from 'supertest';
import Container from 'typedi';
import { deburr } from 'lodash';

const ormClient: IOrmClient = Container.get<IOrmClient>(SequelizeClient);
const httpServer: HttpServer = new HttpServer();

ormClient.connectDatabase();
httpServer.init();

routes(httpServer.getApplication());
httpServer.run();
        
const request: supertest.SuperTest<supertest.Test> = supertest(httpServer.getApplication());

describe('GET /suggestions', function() {
  describe('with a non-existent city', function () {
    let response: Response;

    before(function (done) {
      request
        .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .end(function (err, res) {
          response = res;
          response.body.json = JSON.parse(res.text);
          done(err);
        });
    });

    after(function (done) {
      httpServer.getInstance().close();
      done();
    });

    it('returns a 200', function () {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an empty array of suggestions', function () {
      expect(response.body.json.suggestions).to.be.instanceof(Array);
      expect(response.body.json.suggestions).to.have.length(0);
    });
  });

  describe('with a valid city', function () {
    let response: Response;

    before(function (done) {
      request
        .get('/suggestions?q=Montreal')
        .end(function (err, res) {
          response = res;
          response.body.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', function () {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', function () {
      expect(response.body.json.suggestions).to.be.instanceof(Array);
      expect(response.body.json.suggestions).to.have.length.above(0);
    });

    it('contains latitudes and longitudes', function () {	
      expect(response.body.json.suggestions).to.satisfy(function (suggestions: any[]) {	
        return suggestions.every(function (suggestion) {	
          return suggestion.latitude && suggestion.longitude;	
        });	
      })	
    });	

    it('contains scores', function () {	
      expect(response.body.json.suggestions).to.satisfy(function (suggestions: any[]) {	
        return suggestions.every(function (suggestion) {	
          return suggestion.latitude && suggestion.longitude;	
        });	
      })	
    });

    it('contains a match', function () {
      expect(response.body.json.suggestions).to.satisfy(function (suggestions: any[]) {
        return suggestions.some(function (suggestion) {
          return /montreal/i.test(deburr(suggestion.name));
        });
      })
    });
  });
});
