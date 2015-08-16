Application
===========

The application can be accessed from both JavaScript and an HTTP API
endpoint.

JavaScript
----------

The application is implemented as a JavaSCript function, by [exposing a
predefined](suggestions.js) [suggestions engine](suggestions), using
cities from Canada and USA, a trie matcher, and a scoring engine based
on key length (closest to the query length is better), population
(higher is better), and distance from an optional point given in the
query object.

A slugification function is applied before matching to make the matching
insensitive to case, accents, spaces, etc.

HTTP
----

An [HTTP API endpoint](app.js) is implemented using the Express
framework, and is basically proxying the request query on `/suggestions`
to the above JavaScript API.
