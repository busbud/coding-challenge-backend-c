/* eslint-disable import/first */

const ES_PORT = '9200';
process.env.ES_PORT = ES_PORT;

// Functions
const mockBulk = jest.fn();
const mockSearch = jest.fn();
const mockIndicesExists = jest.fn();
const mockIndicesCreate = jest.fn();

// Dependencies
const mockEsClient = jest.fn().mockImplementation(() => ({
  bulk: mockBulk,
  search: mockSearch,
  indices: {
    exists: mockIndicesExists,
    create: mockIndicesCreate,
  },
}));

import { QueryDslQueryContainer, SearchSortContainerKeys } from '@elastic/elasticsearch/api/types';
import { ElasticSearchClient } from '../elasticsearch-client';

jest.mock('@elastic/elasticsearch', () => ({
  Client: mockEsClient,
}));

let elasticSearchClient: ElasticSearchClient;

describe('ElasticSearchClient', () => {
  beforeEach(() => {
    elasticSearchClient = new ElasticSearchClient();
  });

  describe('constructor', () => {
    it('should construct using default values', () => {
      const client = new ElasticSearchClient();
      expect(mockEsClient).toHaveBeenCalledWith({ node: `http://localhost:${ES_PORT}` });
    });

    it('should construct using option values', () => {
      const mockedHost = '_host_';
      const client = new ElasticSearchClient({ node: mockedHost });
      expect(mockEsClient).toHaveBeenCalledWith({ node: mockedHost });
    });
  });

  describe('isExistentIndex', () => {
    it('should return true', async () => {
      const mockedIndex = '_index_';
      mockIndicesExists.mockResolvedValueOnce({ body: true });

      const result = await elasticSearchClient.isExistentIndex(mockedIndex);

      expect(mockIndicesExists).toHaveBeenCalledWith({ index: mockedIndex });
      expect(result).toBe(true);
    });

    it('should return false', async () => {
      const mockedIndex = '_index_';
      mockIndicesExists.mockResolvedValueOnce({ body: false });

      const result = await elasticSearchClient.isExistentIndex(mockedIndex);

      expect(mockIndicesExists).toHaveBeenCalledWith({ index: mockedIndex });
      expect(result).toBe(false);
    });
  });

  describe('createIndex', () => {
    it('should create index', async () => {
      const options = { index: '_index_' };
      mockIndicesCreate.mockResolvedValueOnce({ statusCode: 200 });

      const result = await elasticSearchClient.createIndex(options);

      expect(mockIndicesCreate).toHaveBeenCalledWith(options);
      expect(result).toBe(200);
    });
  });

  describe('bulkInsert', () => {
    it('should bulk insert without specifying _id', async () => {
      const index = '_index_';
      const data = [{ city: 'one' }, { city: 'two' }];

      const expected = data.flatMap((item) => [
        { index: { _index: index } },
        item,
      ]);

      mockBulk.mockResolvedValueOnce({ body: { errors: false }, statusCode: 200 });

      const result = await elasticSearchClient.bulkInsert(index, data);

      expect(mockBulk).toHaveBeenCalledWith({ body: expected });
      expect(result).toEqual({ errors: false, statusCode: 200 });
    });

    it('should bulk insert with specified _id', async () => {
      const index = '_index_';
      const data = [{ city_id: 'city_id1', city: 'one' }, { city_id: 'city_id2', city: 'two' }];

      const expected = data.flatMap((item) => [
        { index: { _index: index, _id: item.city_id } },
        item,
      ]);

      mockBulk.mockResolvedValueOnce({ body: { errors: false }, statusCode: 200 });

      const result = await elasticSearchClient.bulkInsert(index, data, 'city_id');

      expect(mockBulk).toHaveBeenCalledWith({ body: expected });
      expect(result).toEqual({ errors: false, statusCode: 200 });
    });
  });

  describe('searchAsYouType', () => {
    it('should search without options', async () => {
      const index = '_index_';
      const field = '_field_';
      const term = '_term_';
      const expected = {
        index,
        body: {
          query: {
            multi_match: {
              query: term,
              type: 'bool_prefix',
              fields: [field, `${field}.2gram`, `${field}.3gram`],
            },
          },
          sort: {},
          size: 10,
        },
      };

      mockSearch.mockResolvedValueOnce({ body: { hits: { hits: [] } } });

      const results = await elasticSearchClient.searchAsYouType(index, field, term);

      expect(mockSearch).toHaveBeenCalledWith(expected);
      expect(results).toEqual({ hits: [] });
    });

    it('should search including options', async () => {
      const index = '_index_';
      const field = '_field_';
      const term = '_term_';

      // Mocked options
      const mockedOptions: { sort: SearchSortContainerKeys, query: QueryDslQueryContainer } = {
        sort: {
          _geo_distance: {
            location: {
              lat: 0,
              lon: 0,
            },
            order: 'asc',
          },
        },
        query: {
          match: {
            name: '_term_',
          },
        },
      };

      const expected = {
        index,
        body: {
          query: {
            multi_match: {
              query: term,
              type: 'bool_prefix',
              fields: [field, `${field}.2gram`, `${field}.3gram`],
            },
            ...mockedOptions.query,
          },
          sort: {
            ...mockedOptions.sort,
          },
          size: 10,
        },
      };

      mockSearch.mockResolvedValueOnce({ body: { hits: { hits: [] } } });

      const results = await elasticSearchClient.searchAsYouType(index, field, term, mockedOptions);

      expect(mockSearch).toHaveBeenCalledWith(expected);
      expect(results).toEqual({ hits: [] });
    });
  });
});
