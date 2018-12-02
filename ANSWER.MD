# TODO

[ sort index for max reuse ][ sanitiez user input and indexing phase on name]
[add tests][support geodistance sorting]
[support and benchmark from index]

# Description

All the code was written from scratch for the sake of the example, especially, the functionnal utils (curry), the server layer(express-like).
The implementations are of course very simple. In Real life though, a mature lib as express or fastify should be used instead.

# Gotchas

To simplify code writing and use recent additions in the es6 standard, the latest nodejs LTS has been used.
The 0.10.26 is vulnerable to multiple attacks:

https://www.cvedetails.com/vulnerability-list/vendor_id-12113/product_id-30764/version_id-192865/Nodejs-Node.js-0.10.26.html

- also jest is recommended to replace mocha + chai + sinon, as it integrates all tools in one single place + snapshot testing, yeah !.
- Coverage has been added to help follow the remaining code to be tested
