import { Test, TestingModule } from '@nestjs/testing';
import { LocationParser } from './location.parser';

describe('LocationService', () => {
  let service: LocationParser;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [LocationParser],
    }).compile();

    service = module.get<LocationParser>(LocationParser);
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
