import { Suggestion } from './interfaces/suggestion';
import { CityQueryResult } from '../cities';
import { SuggestionMapper } from './suggestion.mapper';

describe('SuggestionMapper', () => {
  const city: CityQueryResult = {
    geohash: 'aGeohash',
    population: 12341,
    searchScore: 0.13,
    name: 'aName',
    location: {
      lng: 45.123,
      lat: -65.234,
    },
    country: 'aCountry',
    state: 'aState',
    normalized_name: 'aNormalizedName',
    alt_name: 'a',
    id: '123123',
  };
  let suggestion: Suggestion;
  let mapper: SuggestionMapper;

  beforeEach(() => {
    mapper = new SuggestionMapper({
      getScore() {
        return 0.123;
      },
    });
    suggestion = mapper.toSuggestion(city);
  });

  it('should generate a random unique id', () => {
    expect(typeof suggestion.id).toBe('string');
    const another = mapper.toSuggestion(city);
    expect(another.id).not.toBe(suggestion.id);
  });

  it('should compute score', () => {
    expect(suggestion.score).toBe(0.123);
  });

  it('should generate name correctly', () => {
    expect(suggestion.name).toBe('aName, aState, aCountry');
  });

  it('should convert cords to string', () => {
    expect(suggestion.longitude).toBe('45.123');
    expect(suggestion.latitude).toBe('-65.234');
  });

  it('should contains only needed fields', () => {
    expect(Object.keys(suggestion).sort()).toEqual([
      'id',
      'latitude',
      'longitude',
      'name',
      'score',
    ]);
  });
});
