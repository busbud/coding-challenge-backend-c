import { InternalErrorException } from "@domain/exceptions/internal.exception";

const fuzzySearch = require('fuzzy-search');
export class FuzzySearchSharedService {
    filter(searchParam: string, data: any[], propsToBeSearched: string[]):Promise<any[]> {
        try {
          const searcher = new fuzzySearch(data, propsToBeSearched, {
            caseSensitive: false,
          });
          const result = searcher.search(searchParam);
          return result;
        } catch (error) {
          throw new InternalErrorException(error);
        }
      }
}
