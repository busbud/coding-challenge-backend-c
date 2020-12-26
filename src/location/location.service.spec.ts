import { Test, TestingModule } from '@nestjs/testing';
import {
  LOCATION_CONFIG_INJECTION_TOKEN,
  LocationService,
} from './location.service';

describe('LocationService', () => {
  let service: LocationService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        LocationService,
        {
          provide: LOCATION_CONFIG_INJECTION_TOKEN,
          useValue: {},
        },
      ],
    }).compile();

    service = module.get<LocationService>(LocationService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });

  it('should return undefined if at least one is empty', () => {
    expect(service.parse(undefined)).toEqual(undefined);
    expect(service.parse(45.123, undefined)).toEqual(undefined);
  });

  it('should parse location', () => {
    expect(service.parse(44.456, 45.123)).toEqual({ lat: 44.456, lng: 45.123 });
  });
});
