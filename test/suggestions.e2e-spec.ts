import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication, ValidationPipe } from '@nestjs/common';
import request from 'supertest';
import { AppModule } from '../src/app.module';
import { waitUntilHealthy } from './util';
import { Suggestion } from '../src/sugesstions';
import { FastifyAdapter } from '@nestjs/platform-fastify';

describe('SuggestionsController (e2e)', () => {
  let app: INestApplication;
  const search = (q: string, limit?: number) =>
    request(app.getHttpServer()).get(
      limit ? `/suggestions?q=${q}&limit=${limit}` : `/suggestions?q=${q}`,
    );

  beforeAll(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();

    app = moduleFixture.createNestApplication(new FastifyAdapter());
    app.useGlobalPipes(new ValidationPipe({ transform: true }));
    await app.init();
    await app.getHttpAdapter().getInstance().ready();
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
    let response: request.Response;
    let body;

    beforeEach((done) => {
      search('Montreal').then((res) => {
        response = res;
        body = res.body;
        done();
      });
    });

    it('should return a 200', () => {
      expect(response.status).toBe(200);
    });

    it('should return an array of suggestions', () => {
      expect(body).toBeTruthy();
      expect(body.suggestions).toBeInstanceOf(Array);
      expect(body.suggestions.length).toEqual(5);
    });

    it('contains latitudes and longitudes', () => {
      expect(
        body.suggestions.every(
          (suggestion) => suggestion.latitude && suggestion.longitude,
        ),
      ).toBe(true);
    });

    it('contains scores', () => {
      expect(
        body.suggestions.every(
          (suggestion) =>
            typeof suggestion.score === 'number' &&
            suggestion.score > 0 &&
            suggestion.score <= 1,
        ),
      ).toBe(true);
    });

    it('contains sorted scores', () => {
      expect(
        body.suggestions.every(
          (suggestion, index) =>
            index < 1 || suggestion.score < body.suggestions[index - 1].score,
        ),
      ).toBe(true);
    });

    it('contains a match', () => {
      expect(body.suggestions.map((s) => s.name)).toContain(
        'Montréal, QC, Canada',
      );
    });
  });

  describe('with different request limits', () => {
    it('should return 5 elements by default', () =>
      search('mon')
        .expect(200)
        .expect(({ body }) => {
          expect(body.suggestions).toHaveLength(5);
        }));

    it('should return 10 elements', () =>
      search('mon', 10)
        .expect(200)
        .expect(({ body }) => {
          expect(body.suggestions).toHaveLength(10);
        }));

    it('should return an error when limit exceeded', () =>
      search('mon', 100).expect(400));
  });

  describe('with location', () => {
    let lastElement: Suggestion;

    beforeEach(() =>
      search('new', 5).then(({ body }) => {
        expect(body.suggestions.length).toBe(5);
        lastElement = body.suggestions[4];
      }),
    );
    it("should improve the city's score considerately", () =>
      request(app.getHttpServer())
        .get(
          `/suggestions?q=new&limit=5&longitude=${lastElement.longitude}&latitude=${lastElement.latitude}`,
        )
        .expect(200)
        .expect(({ body }) => {
          expect(
            body.suggestions.findIndex((s) => s.name === lastElement.name),
          ).toBeLessThan(4);
        }));
  });
});
