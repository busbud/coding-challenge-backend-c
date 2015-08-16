'use strict';

import _                from 'lodash';
import {build}          from './matching';
import SuggestionsError from './error';

// Decorate a suggestion engine to validate its parameters
const validated = f => query => {
  if (!query) {
    throw new SuggestionsError('Suggestion engine expects one argument.');
  }

  if (!('q' in query)) {
    throw _.assign(
      new SuggestionsError('Missing required parameter `q`.'),
      {status: 400}
    );
  }

  return f(query);
};

/**
 * Build a suggestion engine.
 *
 * @param  {Function} key     A function to extract/compute the comparison key
 *                            of an item.
 * @param  {Function} matcher A matcher (see `matching`).
 * @param  {Function} scoring A scoring engine (see `scoring`).
 * @param  {Object[]} items   An item set to base suggestions on.
 * @return {Function}         The actual suggestion engine.
 */
export default function engine(key, matcher, scoring, items) {
  // Compute the matching key
  items = build(key, items);

  // Instanciate given matcher with this item set
  const match = matcher(items);

  // Get matching items, then compute a score
  return validated(query => {
    const filtered = match(query.q);

    if (!filtered.length) {
      return filtered;
    }

    const score = scoring(query, filtered);

    return _(filtered)
      .map(i => _.assign({score: score(i)}, i))
      .sortByOrder('score', 'desc')
      .value();
  });
}

/**
 * Build a suggestion engine with a normalization function applied before
 * comparison.
 *
 * It will apply `f` to the `key` function result and to the `query` parameter
 * when the engine is called.
 */
export const normalized = (f, buildEngine = engine) =>
  (key, ...args) =>
    _.compose(
      buildEngine(_.compose(f, key), ...args),
      validated(query => _.assign({}, query, {q: f(query.q)}))
    );
