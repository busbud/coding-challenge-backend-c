import { CitiesOrderedByLocUseCase } from '@application/usecases/suggestions/cities.ordered.by.loc.use.case';
import { LocationSharedService } from '@domain/shared/services/location.shared.service';
import { mockedDataICity } from '@tests/__mocks__/suggestions/city';

describe('CitiesOrderedByLocUseCase', () => {

  const mockLocationSharedService = LocationSharedService as jest.Mock<LocationSharedService>;
  const mockedLocationSharedService = new mockLocationSharedService() as jest.Mocked<LocationSharedService>;

  const mockCitiesOrderedByLocUseCase = CitiesOrderedByLocUseCase as jest.Mock<CitiesOrderedByLocUseCase>;
  const mockedCitiesOrderedByLocUseCase = new mockCitiesOrderedByLocUseCase(mockedLocationSharedService) as jest.Mocked<CitiesOrderedByLocUseCase>;

  describe('get suggestions', () => {
    it('should get cities suggestions with scores by loc and name', async () => {
      const mockLatitude = '43.70011';
      const mockLongitude = '-79.4163';
      
      const sut = await mockedCitiesOrderedByLocUseCase.execute(mockedDataICity, mockLatitude, mockLongitude);

      expect(sut).not.toBeUndefined();
    });
  });
});