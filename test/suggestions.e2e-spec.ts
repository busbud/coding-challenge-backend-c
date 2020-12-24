import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import request from 'supertest';
import { AppModule } from '../src/app.module';
import { waitUntilHealthy } from './util';

describe('SuggestionsController (e2e)', () => {
  let app: INestApplication;

  beforeAll(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();

    app = moduleFixture.createNestApplication();
    await app.init();
    await waitUntilHealthy(app);
  });

  describe('with a non-existent city', () => {
    let test: request.Test;

    beforeEach(() => {
      test = request(app.getHttpServer()).get(
        '/suggestions?q=SomeRandomCityInTheMiddleOfNowhere',
      );
    });

    it('should return a 404', () => test.expect(404));

    it('should return an empty array of suggestions', () =>
      test.expect({ suggestions: [] }));
  });

  describe('with a valid city', () => {
    let test: request.Test;

    beforeEach(() => {
      test = request(app.getHttpServer()).get('/suggestions?q=Montreal');
    });

    it('should return a 200', () => test.expect(200));

    it('should return an array of suggestions', () =>
      test.expect((res) => {
        expect(res.body).toBeTruthy();
        expect(res.body.suggestions).toBeInstanceOf(Array);
        expect(res.body.suggestions.length).toEqual(10);
      }));
  });
});
