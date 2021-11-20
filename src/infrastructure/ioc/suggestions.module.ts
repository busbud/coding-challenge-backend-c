import { Module } from '@nestjs/common';
import { GetCitySuggestionsController } from '@presentation/controllers/suggestions/get.city.suggestions.controller';
import { FuzzySearchSharedService } from '@domain/shared/services/fuzzy.search.service';
import { LocationSharedService } from '@domain/shared/services/location.shared.service';
import { CitiesOrderedByLocUseCase } from '@application/usecases/suggestions/cities.ordered.by.loc.use.case';
import { GetCitySuggestionsUseCase } from '@application/usecases/suggestions/get.city.suggestions.use.case';
import { CitiesOrderedByNameUseCase } from '@application/usecases/suggestions/cities.ordered.by.name.use.case';
@Module({
  imports: [],
  controllers: [GetCitySuggestionsController],
  providers: [
    CitiesOrderedByNameUseCase, CitiesOrderedByLocUseCase, GetCitySuggestionsUseCase, FuzzySearchSharedService, LocationSharedService
  ],
})
export class SuggestionsModule {}

