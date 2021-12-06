/* eslint-disable import/first */

const mockSearchAsYouType = jest.fn();

import { SuggestionService } from '../suggestion-service';
import { ElasticSearchClient } from '../../clients/elasticsearch-client';

jest.mock('../../clients/elasticsearch-client', () => ({
  ElasticSearchClient: jest.fn().mockImplementation(() => ({
    searchAsYouType: mockSearchAsYouType,
  }))
}));

const mockedEsClient = new ElasticSearchClient();
let suggestionService: SuggestionService;

describe('SuggestionService', () => {
  beforeEach(() => {
    suggestionService = new SuggestionService(mockedEsClient);
  });

  describe('getSuggestions', () => {
    it('should call ES without location', async () => {
      const term = '_term_';
      const mockedHits = {
        hits: [
          {
            _source: {
              name_concat: '_name_concat_',
              location: {
                lat: 1,
                lon: 2,
              }
            },
            _score: 1
          }
        ]
      }
      const expectedResult = [{ name: '_name_concat_', latitude: 1, longitude: 2, score: 1 }];

      mockSearchAsYouType.mockResolvedValueOnce(mockedHits);
      const result = await suggestionService.getSuggestions(term);

      expect(mockSearchAsYouType).toHaveBeenCalledWith('cities', 'name_concat', term, {});
      expect(result).toEqual(expectedResult);
    });

    it('should call ES with location', async () => {
      const term = '_term_';
      const location = { lat: 1, long: 2 };
      const mockedHits = {
        hits: [
          {
            _source: {
              name_concat: '_name_concat_',
              location: {
                lat: 1,
                lon: 2,
              }
            },
            _score: null,
            sort: [1.2345]
          }
        ]
      }
      const expectedOptions = {
        sort: {
          _geo_distance: {
            location: {
              lat: location.lat,
              lon: location.long,
            },
            order: 'asc',
            unit: 'km',
          },
        }
      };

      const expectedResult = [{ name: '_name_concat_', latitude: 1, longitude: 2, score: 1.2345 }];

      mockSearchAsYouType.mockResolvedValueOnce(mockedHits);
      const result = await suggestionService.getSuggestions(term, location);

      expect(mockSearchAsYouType).toHaveBeenCalledWith('cities', 'name_concat', term, expectedOptions);
      expect(result).toEqual(expectedResult);
    });
  });
});
