import { Injectable, Logger } from '@nestjs/common';
import { SuggestionsEvents } from '../app-events';
import { OnEvent } from '@nestjs/event-emitter';
import { Suggestion } from '../sugesstions';
import { CityQueryResult } from '../cities';

@Injectable()
export class DebugService {
  private readonly logger = new Logger(DebugService.name);
  private generated: Record<string, CityQueryResult> = {};

  @OnEvent(SuggestionsEvents.SUGGESTION_GENERATED)
  storeSource(s: Suggestion, from: CityQueryResult) {
    this.generated[s.id] = from;
  }

  @OnEvent(SuggestionsEvents.SUGGESTION_RETURNED)
  logReturned(s: Suggestion) {
    const from = this.generated[s.id];
    this.logger.log(
      `RETURNED ${JSON.stringify(s, null, 2)} ${JSON.stringify(from, null, 2)}`,
    );
    delete this.generated[s.id];
  }
}
