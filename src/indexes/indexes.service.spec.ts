import { Test, TestingModule } from '@nestjs/testing';
import { EventEmitter2, EventEmitterModule } from '@nestjs/event-emitter';
import { IndexesEvents } from '../app-events';
import { IndexesRepository } from './indexes.repository';
import { IndexesService } from './indexes.service';
import any = jasmine.any;

describe('IndexesService', () => {
  let service: IndexesService;
  let events: EventEmitter2;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      imports: [EventEmitterModule.forRoot()],
      providers: [IndexesRepository, IndexesService],
    }).compile();

    service = module.get<IndexesService>(IndexesService);
    events = module.get<EventEmitter2>(EventEmitter2);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });

  it('should emit indexes at bootstrap', (done) => {
    const spy = spyOn(events, 'emit').and.callThrough();
    events.addListener(IndexesEvents.NEW_INDEXES, () => {
      expect(spy).toHaveBeenCalledWith(
        IndexesEvents.NEW_INDEXES,
        'cities',
        any(Array),
      );
      done();
    });
    service.onApplicationBootstrap();
  });
});
