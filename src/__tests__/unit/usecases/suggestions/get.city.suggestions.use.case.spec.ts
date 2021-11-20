import { CitiesOrderedByLocUseCase } from '@application/usecases/suggestions/cities.ordered.by.loc.use.case';
import { CitiesOrderedByNameUseCase } from '@application/usecases/suggestions/cities.ordered.by.name.use.case';
import { GetCitySuggestionsUseCase } from '@application/usecases/suggestions/get.city.suggestions.use.case';
import { FuzzySearchSharedService } from '@domain/shared/services/fuzzy.search.service';
import { LocationSharedService } from '@domain/shared/services/location.shared.service';
import { mockedDataICity } from '@tests/__mocks__/suggestions/city';
import { mockedDataICitySuggestions } from '@tests/__mocks__/suggestions/city.suggestions';

describe('GetCitySuggestionsUseCase', () => {
  const mockFuzzySearchSharedService = FuzzySearchSharedService as jest.Mock<FuzzySearchSharedService>;
  const mockedFuzzySearchSharedService = new mockFuzzySearchSharedService() as jest.Mocked<FuzzySearchSharedService>;

  const mockCitiesOrderedByNameUseCase = CitiesOrderedByNameUseCase as jest.Mock<CitiesOrderedByNameUseCase>;
  const mockedCitiesOrderedByNameUseCase = new mockCitiesOrderedByNameUseCase() as jest.Mocked<CitiesOrderedByNameUseCase>;

  const mockLocationSharedService = LocationSharedService as jest.Mock<LocationSharedService>;
  const mockedLocationSharedService = new mockLocationSharedService() as jest.Mocked<LocationSharedService>;

  const mockCitiesOrderedByLocUseCase = CitiesOrderedByLocUseCase as jest.Mock<CitiesOrderedByLocUseCase>;
  const mockedCitiesOrderedByLocUseCase = new mockCitiesOrderedByLocUseCase(mockedLocationSharedService) as jest.Mocked<CitiesOrderedByLocUseCase>;

  const mockGetCitySuggestionsUseCase = GetCitySuggestionsUseCase as jest.Mock<GetCitySuggestionsUseCase>;
  const mockedGetCitySuggestionsUseCase = new mockGetCitySuggestionsUseCase(mockedCitiesOrderedByNameUseCase, mockedCitiesOrderedByLocUseCase, mockedFuzzySearchSharedService) as jest.Mocked<GetCitySuggestionsUseCase>;

  describe('get suggestions', () => {
    it('should get cities suggestions with scores by loc', async () => {
      jest.spyOn(mockedCitiesOrderedByLocUseCase, 'execute').mockImplementationOnce(async () => mockedDataICitySuggestions);
      jest.spyOn(mockedFuzzySearchSharedService, 'filter').mockImplementationOnce(async () => mockedDataICity);

      const sut = await mockedGetCitySuggestionsUseCase.execute({ q: 'London', latitude: '43.70011', longitude: '-79.4163' });

      expect(sut).not.toBeUndefined();
      expect(sut).toEqual(mockedDataICitySuggestions);
    });

    it('should get cities suggestions with scores by name', async () => {
      jest.spyOn(mockedCitiesOrderedByNameUseCase, 'execute').mockImplementationOnce(async () => mockedDataICitySuggestions);
      jest.spyOn(mockedFuzzySearchSharedService, 'filter').mockImplementationOnce(async () => mockedDataICity);

      const sut = await mockedGetCitySuggestionsUseCase.execute({ q: 'London', latitude: undefined, longitude: undefined });

      expect(sut).not.toBeUndefined();
      expect(mockedCitiesOrderedByNameUseCase.execute).toHaveBeenCalled();
      expect(mockedFuzzySearchSharedService.filter).toHaveBeenCalled();
      expect(sut).toEqual(mockedDataICitySuggestions);
    });

    it('should throw an error when lat or long arent sent', async () => {
      try {
        await mockedGetCitySuggestionsUseCase.execute({ q: 'London', latitude: '43.70011', longitude: undefined });
      } catch (error) {
        expect(error).not.toBeUndefined();
        expect(error.status).toEqual(400);
      }
    });
  });
});