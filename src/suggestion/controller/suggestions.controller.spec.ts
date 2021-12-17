import {INestApplication} from '@nestjs/common';
import {Test} from '@nestjs/testing';
import * as request from 'supertest';
import {SuggestionModule} from '../../suggestion/suggestion.module';
import {CityModule} from '../../city/city.module';
import {AppModule} from '../../app.module';
import {SuggestionService} from '../../suggestion/service/suggestion.service';
import connection from '../utils/connection';
import 'reflect-metadata';

describe('Suggestions', () => {
    let app: INestApplication;
    const suggestionService = {findSuggestionsWithParams: () => ['test']};

    afterAll(async () => {
        await connection.close();
    });

    beforeEach(async () => {
        await connection.clear();
    });

    beforeAll(async () => {
        await connection.create();

        const moduleRef = await Test.createTestingModule({
            imports: [SuggestionModule, CityModule, AppModule],
        })
            .overrideProvider(SuggestionService)
            .useValue(suggestionService)
            .compile();

        app = moduleRef.createNestApplication();
        await app.init();

    });

    it(`/GET suggestions valid name`, () => {
        return request(app.getHttpServer())
            .get('/suggestions?q=Montr')
            .expect(200)
            .expect({
                data: suggestionService.findSuggestionsWithParams(),
            });
    });

    it(`/GET suggestions valid name and coordinates`, () => {
        return request(app.getHttpServer())
            .get('/suggestions?q=Londo&latitude=48.70011&longitude=-73.4163')
            .expect(200)
            .expect({
                data: suggestionService.findSuggestionsWithParams(),
            });
    });

    it(`/GET suggestions invalid name`, () => {
        return request(app.getHttpServer())
            .get('/suggestions?q=fdfsdfs')
            .expect(404)
            .expect({
                data: suggestionService.findSuggestionsWithParams(),
            });
    });

    afterAll(async () => {
        await app.close();
    });
});
