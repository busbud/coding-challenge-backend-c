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
    it('should call ES without location', () => {
      const term = '_term_';
      
      suggestionService.getSuggestions(term);
      
      expect(mockSearchAsYouType).toHaveBeenCalledWith('cities', 'name_concat', term, {});
    });

    it('should call ES with location', () => {
      const term = '_term_';
      const location = { lat: 1, long: 2 };
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
      }

      suggestionService.getSuggestions(term, location);
      
      expect(mockSearchAsYouType).toHaveBeenCalledWith('cities', 'name_concat', term, expectedOptions);
    });
  });
});
