Scoring
=======

Scoring components for search results, with configurable criteria and
weight.

Interface
---------

Scoring functions ([`distance`](distance.js),
[`length`](length.js), [`population`](population.js)) have a
common interface:

```js
(query: Object, items: Array) => item: Object => score: Number
```

The scoring function is called once when a suggestion query is issued,
and is given the query object, and the matched items.

A function is then returned, taking a single item, and returning a score
between 0 and 1 for this item.

Engine
------

The [scoring engine](engine.js) allows to combine multiple weighted
scoring functions into a new scoring function (same interface) that will
run through all criteria and combine the results.
