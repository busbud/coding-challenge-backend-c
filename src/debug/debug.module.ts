import { Module } from '@nestjs/common';
import { DebugService } from './debug.service';

@Module({
  providers: [DebugService],
})
export class DebugModule {}
