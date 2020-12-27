import { Suggestion } from './interfaces/suggestion';
import SortedArray from 'sorted-array';
import { OperatorFunction } from 'rxjs';
import { map, reduce, tap } from 'rxjs/operators';
import { Logger } from '@nestjs/common';

function SuggestionsSorter() {
  return (a: Pick<Suggestion, 'score'>, b: Pick<Suggestion, 'score'>) =>
    b.score - a.score;
}

export class TopSuggestionsReducer {
  static readonly logger = new Logger(TopSuggestionsReducer.name);
  private readonly sortedArray: SortedArray = new SortedArray(
    [],
    SuggestionsSorter(),
  );
  private readonly limit: number;
  private minScore = 0;
  count = 0;

  private constructor(limit: number) {
    this.limit = limit;
  }

  private insert(s: Pick<Suggestion, 'score'>) {
    this.sortedArray.insert(s);
    if (this.array.length > this.limit) {
      this.array.pop();
    }
    return this.last.score;
  }

  private get last(): Suggestion {
    return this.array[this.array.length - 1];
  }

  update(s: Pick<Suggestion, 'score'>) {
    if (this.array.length < this.limit || s.score > this.minScore) {
      this.insert(s);
      this.minScore = this.last.score;
    }
    this.count++;
    return this;
  }

  get array(): any[] {
    return this.sortedArray.array;
  }

  static keepTopSuggestions<T extends Pick<Suggestion, 'score'>>(
    limit: number,
  ): OperatorFunction<T, T[]> {
    return (s$) =>
      s$.pipe(
        reduce(
          (acc, curr) => acc.update(curr),
          new TopSuggestionsReducer(limit),
        ),
        tap(({ count }) =>
          TopSuggestionsReducer.logger.log(
            `Reduced ${count} results to ${limit}`,
          ),
        ),
        map((tsr) => tsr.array),
      );
  }
}
