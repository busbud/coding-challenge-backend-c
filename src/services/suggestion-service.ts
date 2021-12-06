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

export class SuggestionService {
  constructor(private esClient: ElasticSearchClient) {}

  getSuggestions = async (term: string, location?: { lat: number, long: number }) => {
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

    return this.esClient.searchAsYouType<City>('cities', 'name_concat', term, options);
  };
}
