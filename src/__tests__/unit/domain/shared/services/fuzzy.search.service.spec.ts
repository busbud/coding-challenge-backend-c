import { FuzzySearchSharedService } from '@domain/shared/services/fuzzy.search.service';
import { cities } from '../../../../../../data/cities_canada-usa';

describe('FuzzySearchSharedService', () => {

  const mockFuzzySearchSharedService = FuzzySearchSharedService as jest.Mock<FuzzySearchSharedService>;
  const mockedFuzzySearchSharedService = new mockFuzzySearchSharedService() as jest.Mocked<FuzzySearchSharedService>;

  describe('get suggestions', () => {
    it('should return a filtered array', async () => {
      const sut = await mockedFuzzySearchSharedService.filter('Lond', cities, ['name']);

      expect(sut).not.toBeUndefined();
    });
    
  });
});