string-score
============

[![Build Status](https://travis-ci.org/KenPowers/string-score.svg?branch=master)](https://travis-ci.org/KenPowers/string-score) [![Coverage Status](https://coveralls.io/repos/KenPowers/string-score/badge.svg?branch=master)](https://coveralls.io/r/KenPowers/string-score?branch=master)

A pure (functional) version of [Joshaven Potter's `string_score`][ss] package
which doesn't alter the String prototype.

Usage
-----

This module exports a single function with the following signature:

```
score(target, query[, fuzziness]);
```

`target` is the string to score the `query` against, `query` is the string to
score against the `target`, and `fuzziness` is an optional number between `0`
and `1` indicating how relaxed to be while scoring (`0` being exact matches
required, `1` allowing misspellings, defaults to `0`). The result is a number
between `0` and `1` indicating how well `query` matches `target` (`0` being no
match and `1` being a perfect match).

```js
var score = require('string-score');

// Non-fuzzy scoring
score('hello', 'hello'); // => 1
score('hello', ''); // => 0
score('hello', 'he'); // => 0.71
score('hello', 'hellu'); // => 0
score('hello', 'ello'); // => 0.585
score('hello', 'ullu'); // => 0

// Fuzzy scoring
score('hello', 'hello', 0.5); // => 1
score('hello', '', 0.5); // => 0
score('hello', 'he', 0.5); // => 0.71
score('hello', 'hellu', 0.5); // => 0.5766666666666665
score('hello', 'ello', 0.5); // => 0.585
score('hello', 'ullu', 0.5); // => 0.1125
```

License
-------

```
The MIT License (MIT)

Copyright (c) 2015 Kenneth Powers <ken@kenpowers.net> (http://www.kenpowers.net)
Original implementation Copyright (c) 2015 Joshaven Potter <josh@thinkboxi.com> (http://joshaven.com/)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
```

[ss]: https://github.com/joshaven/string_score "joshaven/string_score"
