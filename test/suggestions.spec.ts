import { INestApplication, ValidationPipe } from '@nestjs/common';
import { Test, TestingModule } from '@nestjs/testing';
import { SuggestionsController } from '.././src/suggestions/suggestions.controller';
import * as request from 'supertest'
import { AppModule } from '../src/app.module';
import { PrismaService } from '../src/prisma.service'
import { HttpModule, HttpService } from '@nestjs/axios';


describe("GET /suggestions", () =>  {
  let app: INestApplication

  beforeAll(async () => {
    //configure nestjs to work with jest testing 
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [AppModule, HttpModule],
    }).compile();

    app = moduleFixture.createNestApplication();
    app.enableShutdownHooks();
    app.useGlobalPipes(new ValidationPipe());
    await app.init();
  });

  afterAll(async () => {
    await app.close();
  });

  describe("with a non-existent city", () => {
    var response;

    beforeAll(function (done) {
        request(app.getHttpServer()).get("/suggestions?q=SomeRandomCityInTheMiddleOfNowhere").end(function (err, res) {  
          console.log(err, res)
          response = res;
          response.json = JSON.parse(res.text);
          console.log(response)
          done(err);
        });
    });

    it("returns a 404", function () {
      expect(response.statusCode).toBe(404);
    });

    it("returns an empty array of suggestions", function () {
      expect(response.json.suggestions).toBeInstanceOf(Array);
      expect(response.json.suggestions).toHaveLength(0);
    });
  });

  describe("with a valid city", function () {
    var response;

    beforeAll(function (done) {
      request(app.getHttpServer()).get("/suggestions?q=Montreal").end(function (err, res) {
        response = res;
        response.json = JSON.parse(res.text);
        done(err);
      });
    });

    it("returns a 200", function () {
      expect(response.statusCode).toEqual(200);
    });

    it("returns an array of suggestions", function () {
      expect(response.json.suggestions).toBeInstanceOf(Array);
      expect(response.json.suggestions).toHaveLength(8);
    });

    describe("Validate the shape of the data being returned", function () {
      it("contains latitudes and longitudes", function () {
        expect(response.json.suggestions[0]).toHaveProperty('latitude')
        expect(response.json.suggestions[0]).toHaveProperty('longitude')
      });

      it("contains scores", function () {
        expect(response.json.suggestions[0]).toHaveProperty('score');
  })

    it("contains a match", function () {
       expect(response.json.suggestions[0].name).toContain('Montréal')
     } 
    )
})
})

})