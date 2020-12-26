import { TopSuggestionsReducer } from './top-suggestions.reducer';
import { from } from 'rxjs';

const scores = new Array(50).fill(0).map((_, i) => ({ score: i }));

const expectTopCharCodes = (limit: number) => (result: any) =>
  expect(result).toStrictEqual(scores.slice(-limit).reverse());

describe('TopSuggestionsReducer', () => {
  it('it should keep top 5 suggestions on ascending array', (done) => {
    const limit = 5;
    from(scores)
      .pipe(TopSuggestionsReducer.keepTopSuggestions(limit))
      .subscribe({
        next: expectTopCharCodes(limit),
        complete: done,
      });
  });
  it('it should keep top 5 suggestions on descending array', (done) => {
    const limit = 5;
    from(scores.slice(0).reverse())
      .pipe(TopSuggestionsReducer.keepTopSuggestions(limit))
      .subscribe({
        next: expectTopCharCodes(limit),
        complete: done,
      });
  });
  it('it should keep top 5 suggestions on mixed array', (done) => {
    const limit = 5;
    from(scores.slice(0).sort((a, b) => (b.score % 2) - (a.score % 2)))
      .pipe(TopSuggestionsReducer.keepTopSuggestions(limit))
      .subscribe({
        next: expectTopCharCodes(limit),
        complete: done,
      });
  });
});
