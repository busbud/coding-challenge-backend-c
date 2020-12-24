import { Module } from '@nestjs/common';
import { LocationParser } from './location.parser';

@Module({
  providers: [LocationParser],
  exports: [LocationParser],
})
export class LocationModule {}
