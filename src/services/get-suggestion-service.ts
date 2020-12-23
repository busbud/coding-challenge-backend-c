import { BadRequestError } from '../errors/bad-request-error';
import { ILocation } from '../modals/location';
import SuggestionRepository from './repository';

interface IFindCityRequest {
  requestQuery: any;
  lat?: any;
  long?: any;
}

class FindCityService {
  public async execute({
    requestQuery,
    lat,
    long,
  }: IFindCityRequest): Promise<ILocation[] | []> {
    const suggestionRepository = new SuggestionRepository();
    try {
      const cities = await suggestionRepository.getData({
        requestQuery,
        lat,
        long,
      });
      return cities || [];
    } catch (error) {
      throw new BadRequestError('No Match found');
    }
  }
}

export default FindCityService;
