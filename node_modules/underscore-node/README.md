What is Underscore for Node.js
==============================

[Underscore for Node.js][1] without cross-browser checks.

This is a ported version of [Underscore][2] targeted for Node.js on V8 JavaScript Engine.

Its purpose is to have the exactly same APIs and functionalities but with better perfomance and smaller code size.

It is accomplished by eliminating unnecessary cross-browser compatibility efforts.

Its APIs can be found in [here][3], and it is [fully tested][4] against the same tests for original Underscore.


How To Use
==========

To install via NPM package:

`npm install underscore-node`


Inside your Node.js project:

`var _ = require('underscore-node');`


Disclaimer
==========

This version is targeted for Node.js on V8 JavaScript Engine only.

It is NOT to be used on browsers or any other JavaScript engines.

This means that there will be no efforts to support different JavaScript versions or behaviours.



[1]: https://github.com/joonho1101/underscore-node "Underscore for Node.js"
[2]: https://github.com/jashkenas/underscore "Underscore"
[3]: http://underscorejs.org/ "Underscore API"
[4]: https://travis-ci.org/joonho1101/underscore-node "Travis CI"
