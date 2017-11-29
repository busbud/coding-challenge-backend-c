import 'regenerator-runtime/runtime';
import Database from './database';
import CitiesRepository from './repository';
import { ArgChecker, ArgSanitizer } from './utils';
import { Mapper } from './mapper/mapper';
const JSONStream = require('JSONStream');
const express = require('express');
const app = express();
const http = require('http');

const port = process.env.PORT || 2345;

console.log('Start busbud API');

// some initialization
if (process.env.NODE_ENV !== 'production') {
  const result = require('dotenv').load();
  if (result.error) {
    throw result.error;
  }
  console.log(result.parsed);
}

const database = new Database(process.env.DATABASE_URL);

app.get('/suggestions', async (req, res) => {
  try {
    console.log('receive a new query with params: ', req.query);
    const query = {
      q: req.query.q,
      latitude: req.query.latitude,
      longitude: req.query.longitude
    };

    // check args
    if (!new ArgChecker().check(query)) {
      res.writeHead(404, { 'Content-Type': 'text/plain' });
      res.end(
        JSON.stringify({
          suggestions: []
        })
      );
      return;
    }

    // sanitize
    new ArgSanitizer().sanitize(query);

    const repo = new CitiesRepository(database);
    res.setHeader('Content-Type', 'application/json');

    const stream = await repo.readStream(query);

    const mapper = new Mapper({ objectMode: true });

    let length = 0;
    stream.on('data', data => {
      length++;
    });
    stream.on('end', () => {
      if (length > 0) {
        res.status(200);
      } else {
        res.status(404).end(
          JSON.stringify({
            suggestions: []
          })
        );
      }
    });

    stream
      .pipe(mapper)
      .pipe(JSONStream.stringify('{\n"suggestions":\n[\n', ',\n', '\n]\n}'))
      .pipe(res);
  } catch (err) {
    console.log(err);
    res.status(400).send('invalid query');
  }
});

app.listen(port, function() {
  console.log(`Busbud API server ${port}!`);
});

module.exports = app;
