import { Test, TestingModule } from '@nestjs/testing';
import { DebugService } from './debug.service';

describe('DebugService', () => {
  let service: DebugService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [DebugService],
    }).compile();

    service = module.get<DebugService>(DebugService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
