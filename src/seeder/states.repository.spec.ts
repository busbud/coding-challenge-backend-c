import { Test, TestingModule } from '@nestjs/testing';
import { StatesRepository } from './states.repository';
import { rxjsTest } from '../../test/util';

describe('StatesRepository', () => {
  let service: StatesRepository;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [StatesRepository],
    }).compile();
    service = module.get<StatesRepository>(StatesRepository);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });

  it(
    'should return same code for US',
    rxjsTest(({ expectObservable }) => {
      expectObservable(service.getStateCode('US', 'ANY')).toBe('(a|)', {
        a: 'ANY',
      });
    }),
  );

  it(
    'should return BC for CA.02',
    rxjsTest(({ expectObservable }) => {
      expectObservable(service.getStateCode('CA', '02')).toBe('(a|)', {
        a: 'BC',
      });
    }),
  );

  it(
    'should throw error for unknown state',
    rxjsTest(({ expectObservable }) => {
      expectObservable(service.getStateCode('CA', 'broken')).toBe(
        '#',
        {},
        'Code broken not found for country CA',
      );
    }),
  );
});
