import alasql from 'alasql';
import { BadRequestError } from '../errors/bad-request-error';
import { ILocation } from '../modals/location';
import isEmpty from '../utils/isEmpty';
const tabFile = 'src/data/cities_canada-usa.tsv';

interface LocationRequest {
  requestQuery: string;
  lat?: string;
  long?: string;
}
class SuggestionRepository {
  public async getData({
    requestQuery,
    lat,
    long,
  }: LocationRequest): Promise<ILocation[] | []> {
    try {
      const d = await alasql([
        [
          `SELECT * FROM tab(?)
          WHERE (name LIKE '${requestQuery}%')
          OR (lat = '${lat}%')
          OR (long = '${long}%')
          ORDER BY [1]`,
          [tabFile],
        ],
      ]);
      if (isEmpty(d[0][0])) {
        return [];
      }
      return d[0];
    } catch (error) {
      throw new BadRequestError('something went wrong while reading data');
    }
  }
}
export default SuggestionRepository;
