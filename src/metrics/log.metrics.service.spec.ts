import { Test, TestingModule } from '@nestjs/testing';
import { LogMetricsService } from './log.metrics.service';
import { EventEmitterModule } from '@nestjs/event-emitter';

describe('LogMetricsService', () => {
  let service: LogMetricsService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      imports: [EventEmitterModule.forRoot()],
      providers: [LogMetricsService],
    }).compile();

    service = module.get<LogMetricsService>(LogMetricsService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
