'use strict';

import {key} from '../matching';

/**
 * Score the key length.
 *
 * The shortest key will have a score of 1, while the longest one will have a
 * score of 0.
 */
function length(query, items) {
  const min = query.length;
  const max = Math.max(...items.map(i => i[key].length));
  const size = max - min;

  return item => (max - item[key].length) / size;
}

export default ({q}, items) =>
  length(q, items);
