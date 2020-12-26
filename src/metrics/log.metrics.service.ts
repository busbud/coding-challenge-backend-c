import { Injectable, Logger } from '@nestjs/common';
import { EventEmitter2 } from '@nestjs/event-emitter';
import { SuggestionsEvents } from '../app-events';

const argToString = (a: any) =>
  typeof a === 'object' ? JSON.stringify(a, null, 2) : a;

@Injectable()
export class LogMetricsService {
  private readonly logger = new Logger(LogMetricsService.name);
  private subscribeTo = [SuggestionsEvents.SUGGESTION_RETURNED];

  constructor(private events: EventEmitter2) {
    this.subscribeTo.forEach((event) => {
      this.events.addListener(event, (...args) =>
        this.postMetric(event, ...args),
      );
    });
  }

  private postMetric(...args: any[]) {
    this.logger.log(`METRIC ${args.map(argToString).join(' ')}`);
  }
}
