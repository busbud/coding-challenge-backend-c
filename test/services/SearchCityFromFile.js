import path from 'path'
import { expect } from 'chai';
import { SearchCityFromFile, testHelpers } from '../../src/services';

import { fixturePath } from '../test_setup';

const { ActsAsSearchable } = testHelpers;
const cityDataFile = path.join(fixturePath, 'search_from_file_data.tsv');

const examples = [
  {
    desc: 'should return score in ordered',
    searchQuery: { q: 'Lon' },
    expectedFn: (actual) => expect(actual).to.be.sortedBy('score', { descending: true }),
  },
  {
    desc: 'should return nearest location first',
    searchQuery: {
      q: 'Lon',
      latitude: 45.53151,
      longitude: -73.51806,
    },
    expectedFn: (actual) => expect(actual).to.be.sortedBy('score', { descending: true }),
  },
];

describe('SearchCityFromFile.js', () => {
  const searchService = new SearchCityFromFile({ dataFile: cityDataFile });
  describe('#act_as_search_service', ActsAsSearchable.actAsSearchService({
    searchService,
    examples,
  }));
});
