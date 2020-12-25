import { Test, TestingModule } from '@nestjs/testing';
import { ReadinessIndicator } from './readiness-indicator.service';
import { EventEmitter2, EventEmitterModule } from '@nestjs/event-emitter';
import { CitiesRepositoryEvents } from '../app-events';

describe('ReadinessIndicator', () => {
  let service: ReadinessIndicator;
  let events: EventEmitter2;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      imports: [EventEmitterModule.forRoot()],
      providers: [ReadinessIndicator],
    }).compile();
    service = module.get<ReadinessIndicator>(ReadinessIndicator);
    events = module.get<EventEmitter2>(EventEmitter2);
    await module.init();
  });

  it('should be defined and falsely initialized', (done) => {
    expect(service).toBeDefined();
    service.isHealthy('cities').catch((err) => {
      expect(err.message).toEqual('cities not ready yet');
      done();
    });
  });

  it('should listen for ready events', async () => {
    events.emit(CitiesRepositoryEvents.CITIES_READY);
    const result = await service.isHealthy('cities');
    expect(result.cities.status).toEqual('up');
  });
});
