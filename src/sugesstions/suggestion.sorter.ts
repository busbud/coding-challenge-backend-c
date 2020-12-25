import { Suggestion } from './interfaces/suggestion';

export function SuggestionsSorter() {
  return (a: Suggestion, b: Suggestion) => b.score - a.score;
}
