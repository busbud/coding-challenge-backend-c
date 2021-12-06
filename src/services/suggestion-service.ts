import { QueryDslQueryContainer, SearchSortContainerKeys } from '@elastic/elasticsearch/api/types';
import { ElasticSearchClient } from '../clients/elasticsearch-client';

export interface City {
  geoname_id: number;
  name: string;
  name_concat: string;
  province: string;
  country: string;
  location: { lat: number, lon: number };
  population: number;
}

interface GetSuggestionResult {
  score: number | null;
  name: string;
  latitude: number;
  longitude: number;
}

export class SuggestionService {
  constructor(private esClient: ElasticSearchClient) {}

  getSuggestions = async (term: string, location?: { lat: number, long: number }): Promise<GetSuggestionResult[]> => {
    // Initialize es search options
    const options: { query?: QueryDslQueryContainer, sort?: SearchSortContainerKeys } = {};

    // Expand suggestions from origin location
    if (location) {
      options.sort = {
        _geo_distance: {
          location: {
            lat: Number(location.lat),
            lon: Number(location.long),
          },
          order: 'asc',
          unit: 'km',
        },
      };
    }

    const result = await this.esClient.searchAsYouType<City>('cities', 'name_concat', term, options);

    return result.hits.map((hit) => {
      const score = options.sort ? Number(hit.sort![0]) : hit._score!;

      return {
        score,
        name: hit._source!.name_concat,
        latitude: hit._source!.location.lat,
        longitude: hit._source!.location.lon,
      };
    });
  };
}
