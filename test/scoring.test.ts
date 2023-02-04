import { SCORE_THRESHOLD, scoreByDistance, scoreByNameSimilarity } from '../src/utils/scoring.util';
import { ICityRawData, IGetCitySuggestion } from '../src/interfaces/interfaces';
import { getCitiesDataFromFile } from '../src/utils/parser/tsv_parser';

describe('scoring test', () => {
  let cityData: ICityRawData[];
  beforeAll(async () => {
    cityData = await getCitiesDataFromFile(['CA', 'US']);
  });

  it('string similarity scores should be greater than scoring threshold', () => {
    const result = scoreByNameSimilarity(cityData, 'Mont');
    result.every((c: IGetCitySuggestion) => {
      expect(c.score).not.toBeLessThanOrEqual(SCORE_THRESHOLD);
    });
  });

  it('distance scores should be greater than scoring threshold', () => {
    const result = scoreByNameSimilarity(cityData, 'Mont');
    scoreByDistance(result, '43.70011', '-79.4163');
    result.every((c: IGetCitySuggestion) => {
      expect(c.score).not.toBeLessThanOrEqual(SCORE_THRESHOLD);
    });
  });
});
