/* eslint-disable import/first */

const mockGetSuggestions = jest.fn();
const mockSetKey = jest.fn();

import { Request, Response } from 'express';
import { SuggestionController } from '../suggestion-controller';
import { SuggestionService } from '../../services/suggestion-service';
import { ElasticSearchClient } from '../../clients/elasticsearch-client';
import { RedisClient } from '../../clients/redis-client';

jest.mock('../../services/suggestion-service', () => ({
  SuggestionService: jest.fn().mockImplementation(() => ({
    getSuggestions: mockGetSuggestions,
  }))
}));

jest.mock('../../clients/elasticsearch-client', () => ({
  ElasticSearchClient: jest.fn().mockImplementation()
}));

jest.mock('../../clients/redis-client', () => ({
  RedisClient: jest.fn().mockImplementation(() => ({
    setKey: mockSetKey
  }))
}));

const mockedEsClient = new ElasticSearchClient();
const mockedRedisClient = new RedisClient();
const mockedSuggestionService: SuggestionService = new SuggestionService(mockedEsClient);
let suggestionController: SuggestionController;

describe('SuggestionController', () => {
  beforeEach(() => {
    jest.resetAllMocks();
    suggestionController = new SuggestionController(mockedSuggestionService, mockedRedisClient);
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
      const location = { latitude: 1, longitude: 2 };

      const mockedResJson = jest.fn();
      const mockedReq = { query: { q: term, ...location } } as unknown as Request;
      const mockedRes = { json: mockedResJson } as unknown as Response;

      mockGetSuggestions.mockResolvedValueOnce([]);

      await suggestionController.getSuggestions(mockedReq, mockedRes);

      expect(mockGetSuggestions).toHaveBeenCalledWith(term, { lat: 1, long: 2 });
      expect(mockedResJson).toHaveBeenCalledWith({ suggestions: [] });
    });

    it('should set cache', async () => {
      const term = '_term_';
      const location = { latitude: 1, longitude: 2 };

      const mockedResJson = jest.fn();
      const mockedReq = { query: { q: term, ...location } } as unknown as Request;
      const mockedRes = { json: mockedResJson } as unknown as Response;

      mockGetSuggestions.mockResolvedValueOnce([{ hey: 'there' }]);

      await suggestionController.getSuggestions(mockedReq, mockedRes);

      expect(mockSetKey).toHaveBeenCalledTimes(1);
      expect(mockSetKey).toHaveBeenCalledWith(`${term}:${location.latitude}:${location.longitude}`, [{ hey: 'there' }]);
    });
  });
});