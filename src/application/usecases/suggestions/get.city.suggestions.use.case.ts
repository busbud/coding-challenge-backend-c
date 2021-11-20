import { FullCoodinatesAreMandatoryException } from '@domain/exceptions/suggestions.exception';
import { ICity } from '@domain/interfaces/i.city';
import { ICitySuggestions } from '@domain/interfaces/suggestions/i.city.suggestions';
import { FuzzySearchSharedService } from '@domain/shared/services/fuzzy.search.service';
import { Injectable } from '@nestjs/common';
import { cities } from '../../../../data/cities_canada-usa';
import { CitiesOrderedByLocUseCase } from './cities.ordered.by.loc.use.case';
import { CitiesOrderedByNameUseCase } from './cities.ordered.by.name.use.case';

@Injectable()
export class GetCitySuggestionsUseCase {
  constructor(
    private readonly citiesOrderedByNameUseCase: CitiesOrderedByNameUseCase,
    private readonly citiesOrderedByLocUseCase: CitiesOrderedByLocUseCase,
    private readonly fuzzySearchSharedService: FuzzySearchSharedService,
  ) {}
  async execute({ q, latitude, longitude }: { q: string; latitude: string; longitude: string }): Promise<ICitySuggestions[]> {
    if ((latitude || longitude) && (!latitude || !longitude)) throw new FullCoodinatesAreMandatoryException();

    let result;
    
    const suggestions: ICity[] = await this.fuzzySearchSharedService.filter(q, cities, ['name']);

    if (latitude && longitude) {
      result = await this.citiesOrderedByLocUseCase.execute(suggestions, latitude, longitude);
    } else {
      result = await this.citiesOrderedByNameUseCase.execute(suggestions, q);
    }

    return result;
  }
}
