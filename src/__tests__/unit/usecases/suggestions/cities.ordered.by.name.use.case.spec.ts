import { CitiesOrderedByNameUseCase } from '@application/usecases/suggestions/cities.ordered.by.name.use.case';
import { mockedDataICity } from '@tests/__mocks__/suggestions/city';

describe('CitiesOrderedByNameUseCase', () => {

  const mockCitiesOrderedByNameUseCase = CitiesOrderedByNameUseCase as jest.Mock<CitiesOrderedByNameUseCase>;
  const mockedCitiesOrderedByNameUseCase = new mockCitiesOrderedByNameUseCase() as jest.Mocked<CitiesOrderedByNameUseCase>;

  describe('get suggestions', () => {
    it('should get cities suggestions with scores by name', async () => {
      const sut = await mockedCitiesOrderedByNameUseCase.execute(mockedDataICity, 'Londo');

      expect(sut).not.toBeUndefined();
    });
  });
});