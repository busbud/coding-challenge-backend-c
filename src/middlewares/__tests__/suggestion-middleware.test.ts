/* eslint-disable import/first */

const mockGetKey = jest.fn();
const mockedResJson = jest.fn();
const mockedNext = jest.fn();
const mockedRes = { json: mockedResJson } as unknown as Response;

import { Request, Response } from 'express';
import { SuggestionMiddleware } from '../suggestion-middleware';
import { RedisClient } from '../../clients/redis-client';

jest.mock('../../clients/redis-client', () => ({
  RedisClient: jest.fn().mockImplementation(() => ({
    getKey: mockGetKey
  }))
}));

const mockedRedisClient = new RedisClient();
let suggestionMiddleware: SuggestionMiddleware;

describe('SuggestionMiddleware', () => {
  beforeEach(() => {
    suggestionMiddleware = new SuggestionMiddleware(mockedRedisClient);
  });

  describe('checkSuggestionCache', () => {
    it('should continue execution if no cached value', async () => {
      const term = '_term_';
      const req = { query: { q: term } } as unknown as Request;

      await suggestionMiddleware.checkSuggestionCache(req, mockedRes, mockedNext);
      expect(mockGetKey).toHaveBeenCalledWith(`${term}:0:0`);
      expect(mockedNext).toHaveBeenCalledTimes(1);
    });

    it('should stop and return response if cached value', async () => {
      const term = '_term_';
      const req = { query: { q: term, lat: 1, long: -1 } } as unknown as Request;

      mockGetKey.mockResolvedValueOnce([{ some: 'city' }]);

      await suggestionMiddleware.checkSuggestionCache(req, mockedRes, mockedNext);
      expect(mockGetKey).toHaveBeenCalledWith(`${term}:1:-1`);
      expect(mockedResJson).toHaveBeenCalledWith({ suggestions: [{ some: 'city' }] });
    });
  });
});
