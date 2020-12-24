import { Test, TestingModule } from '@nestjs/testing';
import { CitiesController } from './cities.controller';
import { CitiesService } from './cities.service';
import { from, Observable } from 'rxjs';
import { City } from './interfaces/city';

class CitiesServiceStub {
  static cities: City[] = [
    {
      alt_name: 'Abbotsford,YXX,Абботсфорд',
      name: 'Abbotsford',
      country: 'CA',
      id: '5881791',
      location: { lat: 49.05798, lng: -122.25257 },
      population: 151683,
    },
    {
      alt_name: "Kollinsvil',Коллинсвиль",
      name: 'Collinsville',
      country: 'US',
      id: '4533909',
      location: { lat: 36.36454, lng: -95.83888 },
      population: 5606,
    },
    {
      alt_name: '',
      name: 'Cranberry Township',
      country: 'US',
      id: '8643098',
      location: { lat: 40.68496, lng: -80.10714 },
      population: 28098,
    },
  ];

  queryCities(): Observable<City> {
    return from(CitiesServiceStub.cities);
  }
}

describe('CitiesController', () => {
  let controller: CitiesController;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        {
          provide: CitiesService,
          useClass: CitiesServiceStub,
        },
      ],
      controllers: [CitiesController],
    }).compile();

    controller = module.get<CitiesController>(CitiesController);
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });

  it('it should query cities', (done) => {
    const cities$ = controller.findAll('aQuery');
    cities$.subscribe((cities) => {
      expect(cities).toEqual(CitiesServiceStub.cities);
      done();
    });
  });

  it('it should return empty set when query not present', (done) => {
    const cities$ = controller.findAll();
    cities$.subscribe((cities) => {
      expect(cities).toEqual([]);
      done();
    });
  });
});
