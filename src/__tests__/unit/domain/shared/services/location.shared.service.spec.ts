import { LocationSharedService } from '@domain/shared/services/location.shared.service';

describe('LocationSharedService', () => {

  const mockLocationSharedService = LocationSharedService as jest.Mock<LocationSharedService>;
  const mockedLocationSharedService = new mockLocationSharedService() as jest.Mocked<LocationSharedService>;

  describe('getHowFarCityIs', () => {
    it('should return distance between locations in KM', async () => {
      
      const sut = await mockedLocationSharedService.getHowFarCityIs(35.12898, -84.4163, 35.12898, -84.4163, 'N');

      expect(sut).not.toBeUndefined();
    });
    
  });
});