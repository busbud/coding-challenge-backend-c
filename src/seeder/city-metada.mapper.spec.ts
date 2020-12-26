import { Test, TestingModule } from '@nestjs/testing';
import { CityMetadataMapper } from './city-metadata.mapper';
import { CountriesRepository } from './countries.repository';
import { StatesRepository } from './states.repository';
import { City } from '../cities';
import { rxjsTest } from '../../test/util';
import { LocationModule } from '../location';

describe('CityMetadataMapper', () => {
  let service: CityMetadataMapper;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      imports: [LocationModule],
      providers: [CountriesRepository, StatesRepository, CityMetadataMapper],
    }).compile();
    service = module.get<CityMetadataMapper>(CityMetadataMapper);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });

  describe('with known state and country', () => {
    let mappedCity: City;
    const city: Omit<City, 'normalized_name'> = {
      state: '02',
      country: 'CA',
      name: 'Nombre con tílde',
      alt_name: 'Otro nombre con tílde',
      population: 500,
      location: {
        lat: 45,
        lng: 45,
      },
      geohash: '',
      id: '123',
    };

    beforeEach(() =>
      service
        .map(city)
        .toPromise()
        .then((c) => (mappedCity = c)),
    );

    it('should not modify fixed fields', () => {
      expect(mappedCity.id).toEqual(city.id);
      expect(mappedCity.location).toEqual(city.location);
      expect(mappedCity.population).toEqual(city.population);
      expect(mappedCity.alt_name).toEqual(city.alt_name);
      expect(mappedCity.name).toEqual(city.name);
    });

    it('should normalize name', () => {
      expect(mappedCity.normalized_name).toEqual('Nombre con tilde');
    });

    it('should replace country code with display name', () => {
      expect(mappedCity.country).toEqual('Canada');
    });

    it('should replace state code with display name', () => {
      expect(mappedCity.state).toEqual('BC');
    });
  });

  it(
    'should catch errors and skip',
    rxjsTest(({ expectObservable }) => {
      expectObservable(service.map({ country: 'ASD' } as City)).toBe('|');
    }),
  );
});
