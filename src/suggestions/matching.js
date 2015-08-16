'use strict';

import {assign} from 'lodash';
import Trie from 'triejs';

// The matching key symbol.
export const key = Symbol('key');

/**
 * Build an item set, using given function to compute the key.
 */
export const build = (f, items) =>
  items.map(i => assign({[key]: f(i)}, i));

/**
 * Match items in a trie data structure for performance.
 */
export function trie(items) {
  const trie = new Trie();
  items.forEach(i => trie.add(i[key], i));
  return query => trie.find(query) || [];
}

/* istanbul ignore next */

/**
 * Match items with the `startsWith` method.
 */
export const dumb = items => query =>
  items.filter(i => i[key].startsWith(query));
