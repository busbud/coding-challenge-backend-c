import * as request from 'supertest';
import { Test } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import { SuggestionsModule } from '@infrastructure/ioc/suggestions.module';
import { GetCitySuggestionsUseCase } from '@application/usecases/suggestions/get.city.suggestions.use.case';
import { mockedDataICitySuggestions } from '@tests/__mocks__/suggestions/city.suggestions';

describe('Suggestions', () => {
  let app: INestApplication;

  const mockGetCitySuggestionsUseCase = GetCitySuggestionsUseCase as jest.Mock<GetCitySuggestionsUseCase>;
  const mockedGetCitySuggestionsUseCase = new mockGetCitySuggestionsUseCase() as jest.Mocked<GetCitySuggestionsUseCase>;

  beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      imports: [SuggestionsModule],
    })
      .overrideProvider(GetCitySuggestionsUseCase)
      .useValue(mockedGetCitySuggestionsUseCase)
      .compile();

    app = moduleRef.createNestApplication();
    await app.init();
  });

  it(`/GET suggestions`, () => {
    
    jest.spyOn(mockedGetCitySuggestionsUseCase, 'execute').mockResolvedValue(mockedDataICitySuggestions);

    return request(app.getHttpServer())
      .get('/api/v1/suggestions?q=London&latitude=43.70011&longitude=-79.4163')
      .expect(200)
      .expect(mockedDataICitySuggestions);
  });

  afterAll(async () => {
    await app.close();
  });
});