/* eslint-disable import/first */

const mockGetSuggestions = jest.fn();

import { Request, Response } from 'express';
import { SuggestionController } from '../suggestion-controller';
import { SuggestionService } from '../../services/suggestion-service';
import { ElasticSearchClient } from '../../clients/elasticsearch-client';

jest.mock('../../services/suggestion-service', () => ({
  SuggestionService: jest.fn().mockImplementation(() => ({
    getSuggestions: mockGetSuggestions,
  }))
}));

jest.mock('../../clients/elasticsearch-client', () => ({
  ElasticSearchClient: jest.fn().mockImplementation()
}));

const mockedEsClient = new ElasticSearchClient();
const mockedSuggestionService: SuggestionService = new SuggestionService(mockedEsClient);
let suggestionController: SuggestionController;

describe('SuggestionController', () => {
  beforeEach(() => {
    suggestionController = new SuggestionController(mockedSuggestionService);
  });

  describe('getSuggestions', () => {
    it('should call suggestion service without location', async () => {
      const term = '_term_';

      const mockedResJson = jest.fn();
      const mockedReq = { query: { q: term } } as unknown as Request;
      const mockedRes = { json: mockedResJson } as unknown as Response;

      mockGetSuggestions.mockResolvedValueOnce([]);

      await suggestionController.getSuggestions(mockedReq, mockedRes);

      expect(mockGetSuggestions).toHaveBeenCalledWith(term, undefined);
      expect(mockedResJson).toHaveBeenCalledWith({ suggestions: [] });
    });

    it('should call suggestion service with location', async () => {
      const term = '_term_';
      const location = { lat: 1, long: 2 };

      const mockedResJson = jest.fn();
      const mockedReq = { query: { q: term, ...location } } as unknown as Request;
      const mockedRes = { json: mockedResJson } as unknown as Response;

      mockGetSuggestions.mockResolvedValueOnce([]);

      await suggestionController.getSuggestions(mockedReq, mockedRes);

      expect(mockGetSuggestions).toHaveBeenCalledWith(term, location);
      expect(mockedResJson).toHaveBeenCalledWith({ suggestions: [] });
    });
  });
});