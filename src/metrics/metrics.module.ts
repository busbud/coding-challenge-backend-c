import { Module } from '@nestjs/common';
import { LogMetricsService } from './log.metrics.service';

@Module({
  providers: [LogMetricsService],
})
export class MetricsModule {}
