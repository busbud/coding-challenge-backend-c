import { Module } from '@nestjs/common';
import {
  SUGGESTIONS_CONFIG_INJECTION_TOKEN,
  SuggestionsService,
} from './suggestions.service';
import { SuggestionsController } from './suggestions.controller';
import { LocationModule } from '../location';
import { CitiesModule } from '../cities';
import config from './config';
import { ConfigService } from '@nestjs/config';

@Module({
  imports: [LocationModule, CitiesModule],
  providers: [
    {
      provide: SUGGESTIONS_CONFIG_INJECTION_TOKEN,
      useFactory: config,
      inject: [ConfigService],
    },
    SuggestionsService,
  ],
  controllers: [SuggestionsController],
})
export class SuggestionsModule {}
