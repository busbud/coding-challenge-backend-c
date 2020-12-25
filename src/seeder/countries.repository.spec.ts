import { Test, TestingModule } from '@nestjs/testing';
import { CountriesRepository } from './countries.repository';
import { rxjsTest } from '../../test/util';

describe('CountriesRepository', () => {
  let service: CountriesRepository;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [CountriesRepository],
    }).compile();
    service = module.get<CountriesRepository>(CountriesRepository);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });

  it(
    'should return USA for US',
    rxjsTest(({ expectObservable }) => {
      expectObservable(service.getCountryName('US')).toBe('(a|)', { a: 'USA' });
    }),
  );

  it(
    'should return Canada for CA',
    rxjsTest(({ expectObservable }) => {
      expectObservable(service.getCountryName('CA')).toBe('(a|)', {
        a: 'Canada',
      });
    }),
  );

  it(
    'should throw error for unknown country',
    rxjsTest(({ expectObservable }) => {
      expectObservable(service.getCountryName('broken')).toBe(
        '#',
        {},
        'Country name not found for broken',
      );
    }),
  );
});
