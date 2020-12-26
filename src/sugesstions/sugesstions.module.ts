import { Module } from '@nestjs/common';
import {
  SUGGESTIONS_CONFIG_INJECTION_TOKEN,
  SuggestionsService,
} from './suggestions.service';
import { SuggestionsController } from './suggestions.controller';
import { LocationModule } from '../location';
import { CitiesModule } from '../cities';

@Module({
  imports: [LocationModule, CitiesModule],
  providers: [
    {
      provide: SUGGESTIONS_CONFIG_INJECTION_TOKEN,
      useValue: {
        reportReturned: true,
      },
    },
    SuggestionsService,
  ],
  controllers: [SuggestionsController],
})
export class SuggestionsModule {}
