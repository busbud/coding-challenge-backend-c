'use strict';

import parse      from 'csv-parse';
import {mapSync}  from 'event-stream';
import JSONStream from 'JSONStream';

process.stdin
  .pipe(parse({delimiter: '\t', quote: '', escape: ''}))
  .pipe(mapSync(r => [r[0], {name: r[1], ascii_name: r[2]}]))
  .pipe(JSONStream.stringifyObject())
  .pipe(process.stdout);
