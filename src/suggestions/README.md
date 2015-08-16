Suggestions
===========

Suggestions engine, to build flexible suggestions systems.

Behavior
--------

An [engine](engine.js) is built from 4 components:

* A function to extract/compute the comparison key of an item.
* A [matcher](#matching).
* A [scoring](scoring) engine.
* An item set to base suggestions on.

From these components, a function with the following interface is
returned:

```js
query: Object => result: Array
```

The `query` parameter is an object with at least a `q` key, containing
the query string. It can contain any other key, that may be used by any
scoring function (for example, the [`distance`](scoring/distance.js)
function allows to add `latitude` and `longitude` keys).

The result array contains the matched items, sorted by descending score.

Under the hood
--------------

When created, the scoring engine will first build a matching engine from
the given item set, using the given key extraction function.

Then, in the returned function, allowing to query the engine, it will
call the matcher with the given query string to filter the items, then
call the scoring engine on the filtered result set, sort then by score,
and return the final result.

Normalization
-------------

The `normalized` helper allows to wrap an engine "constructor" to
automatically normalize the comparison key and the query string before
searching for matches.

It can be useful for example to make the engine case insensitive, or
even more advanced normalization functions (ignore spaces, accents).

Matching
--------

A matcher is a function with the following interface:

```js
items: Array => query: String => matches: Array
```

It's first created with an items array. Each item must have a special
symbol attribute (the exported `key` symbol) containing the key to match
items with. It returns a matching function that takes a query string,
and return an array of items whose key matches the query.

The `build` function allows to compute the `key` symbol on a whole item
set using an extraction function, that takes an item and returns the
key.

Two matchers are provided:

* `trie`, that stores the data in a trie data structure for efficient
  matching.
* `dumb`, that loops over the whole set for any query to find the keys
  that `startsWith` given query.

See the following [benchmark](../../tools/benchmark.js):

```
trie x 308,342 ops/sec ±1.38% (95 runs sampled)
dumb x 165 ops/sec ±1.49% (86 runs sampled)
```
