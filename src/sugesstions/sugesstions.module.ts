import { Module } from '@nestjs/common';
import { SuggestionsService } from './suggestions.service';
import { SuggestionsController } from './suggestions.controller';
import { LocationModule } from '../location';
import { CitiesModule } from '../cities';

@Module({
  imports: [LocationModule, CitiesModule],
  providers: [SuggestionsService],
  controllers: [SuggestionsController],
})
export class SuggestionsModule {}
