import { Module } from '@nestjs/common';
import { IndexesRepository } from './indexes.repository';
import { IndexesService } from './indexes.service';

@Module({
  providers: [IndexesRepository, IndexesService],
})
export class IndexesModule {}
