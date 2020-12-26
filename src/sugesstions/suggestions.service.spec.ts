import { Test, TestingModule } from '@nestjs/testing';
import { from, Observable, of } from 'rxjs';
import { EventEmitter2, EventEmitterModule } from '@nestjs/event-emitter';
import { LocationModule } from '../location';
import {
  SUGGESTIONS_CONFIG_INJECTION_TOKEN,
  SuggestionsService,
} from './suggestions.service';
import { CityQuery, CityQueryResult } from '../cities';
import { CitiesService } from '../cities/cities.service';
import { SuggestionsEvents } from '../app-events';
import { SuggestionMapper } from './suggestion.mapper';
import { TopSuggestionsReducer } from './top-suggestions.reducer';
import { LocationService } from '../location/location.service';
import createSpy = jasmine.createSpy;
import Spy = jasmine.Spy;
import { ConfigModule } from '@nestjs/config';

class CitiesStub {
  static cities = [
    {
      name: 'Montréal',
      location: {
        lat: 45.50884,
        lng: -73.58781,
      },
      geohash: 'f25',
      searchScore: 0.978979,
    },
    {
      name: 'Manhattan',
      location: {
        lat: 40.78343,
        lng: -73.96625,
      },
      geohash: 'dr7',
      searchScore: 0.5675,
    },
    {
      name: 'Montgomery',
      location: {
        lat: 32.36681,
        lng: -86.29997,
      },
      geohash: 'djf',
      searchScore: 0.2344,
    },
  ];
  queryCities(_: CityQuery): Observable<CityQueryResult> {
    return from(CitiesStub.cities as CityQueryResult[]);
  }

  getMaxPopulation(): Observable<number> {
    return of(10000);
  }
}

describe('SuggestionsService', () => {
  let service: SuggestionsService;
  let events: EventEmitter2;
  let locations: LocationService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      imports: [
        EventEmitterModule.forRoot(),
        LocationModule,
        ConfigModule.forRoot({ isGlobal: true }),
      ],
      providers: [
        {
          provide: CitiesService,
          useClass: CitiesStub,
        },
        {
          provide: SUGGESTIONS_CONFIG_INJECTION_TOKEN,
          useValue: {
            reportReturned: true,
            weights: {
              population: 0.3,
              criteria: 0.6,
              nearBy: 0.1,
            },
          },
        },
        SuggestionsService,
      ],
    }).compile();

    service = module.get<SuggestionsService>(SuggestionsService);
    events = module.get<EventEmitter2>(EventEmitter2);
    locations = module.get<LocationService>(LocationService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });

  describe('suggest without location', () => {
    const query = { limit: 2, query: 'query' };
    let generatedSpy: Spy;
    let returnedSpy: Spy;
    let topReducerSpy: Spy;
    const toSuggestion = (city) => ({ score: city.searchScore });

    beforeEach((done) => {
      generatedSpy = createSpy(SuggestionsEvents.SUGGESTION_GENERATED);
      returnedSpy = createSpy(SuggestionsEvents.SUGGESTION_RETURNED);
      events.addListener(SuggestionsEvents.SUGGESTION_GENERATED, generatedSpy);
      events.addListener(SuggestionsEvents.SUGGESTION_RETURNED, returnedSpy);
      spyOn(SuggestionMapper.prototype, 'toSuggestion').and.callFake(
        toSuggestion,
      );
      topReducerSpy = spyOn(
        TopSuggestionsReducer,
        'keepTopSuggestions',
      ).and.callThrough();
      service.suggest(query).subscribe({
        complete: done,
      });
    });

    it('should report generated', () => {
      expect(generatedSpy).toHaveBeenCalledTimes(CitiesStub.cities.length);
      CitiesStub.cities.forEach((city) => {
        expect(generatedSpy).toHaveBeenCalledWith(
          toSuggestion(city),
          city,
          query,
        );
      });
    });

    it('should report returned', () => {
      expect(returnedSpy).toHaveBeenCalledTimes(query.limit);
    });

    it('should keep only best scores the result', () => {
      expect(topReducerSpy).toHaveBeenCalledWith(query.limit);
    });
  });

  describe('suggest with location', () => {
    const query = { limit: 2, query: 'query', location: { lat: 1, lng: 1 } };
    let locationSpy: Spy;

    beforeEach((done) => {
      locationSpy = spyOn(locations, 'geohashEncode').and.callThrough();
      service.suggest(query).subscribe({
        complete: done,
      });
    });

    it('should initialize score calculator with location info', () => {
      expect(locationSpy).toHaveBeenCalledWith(query.location);
    });
  });
});
