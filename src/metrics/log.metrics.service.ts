import { Injectable, Logger } from '@nestjs/common';
import { EventEmitter2 } from '@nestjs/event-emitter';
import { SuggestionsEvents } from '../app-events';

@Injectable()
export class LogMetricsService {
  private readonly logger = new Logger(LogMetricsService.name);
  private subscribeTo = [
    SuggestionsEvents.SUGGESTION_GENERATED,
    SuggestionsEvents.SUGGESTION_RETURNED,
  ];

  constructor(private events: EventEmitter2) {
    this.subscribeTo.forEach((event) => {
      this.events.addListener(event, () => this.postMetric(event));
    });
  }

  private postMetric(event) {
    this.logger.verbose(`Posting metric ${event}`);
  }
}
