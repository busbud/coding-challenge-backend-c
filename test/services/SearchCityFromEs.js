import path from 'path';
import fs from 'fs';
import { expect } from 'chai';
import nock from 'nock';

import { SearchCityFromEs, testHelpers } from '../../src/services';

import { fixturePath } from '../test_setup';

const esDataRes = fs.readFileSync(path.join(fixturePath, 'es_data_mock.json'));
const { ActsAsSearchable } = testHelpers;
const ElasticsearchUrl = 'http://localhost:9200';

const examples = [
  {
    desc: 'should return score in ordered',
    searchQuery: { q: 'London' },
    expectedFn: (actual) => expect(actual).to.be.sortedBy('score', { descending: true })
  },
  {
    desc: 'should return nearest location first',
    searchQuery: {
      q: 'London',
      latitude: 40.16721,
      longitude: -105.10193
    },
    expectedFn: (actual) => expect(actual).to.be.sortedBy('score', { descending: true })
  }
];

describe('SearchCityFromEs', () => {
  let nockScope;
  const searchService = new SearchCityFromEs();

  before(() => {
    if (process.env.NOCK_DISABLE) {
      nock.enableNetConnect();
    } else {
      nockScope = nock(`${ElasticsearchUrl}`);
      nockScope
        .persist()
        .post(`/${searchService.indexName}/_search`)
        .reply(200, esDataRes, { 'Content-Type': 'application/json' });
    }
  });

  after(() => {
    if (!process.env.NOCK_DISABLE) {
      nock.cleanAll();
    }
  });

  describe('#act_as_search_service', ActsAsSearchable.actAsSearchService({
    searchService,
    examples
  }));
});
