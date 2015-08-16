'use strict';

import _           from 'lodash';
import express     from 'express';
import suggestions from './suggestions';

const app = express();

app.get('/suggestions', (req, res) => {
  const result = suggestions(req.query)
    .map(
      c => _(c)
        .assign({name: c.full_name})
        .pick(['name', 'latitude', 'longitude', 'score'])
        .value()
    );

  if (!result.length) {
    res.status(404);
  }

  res.json({suggestions: result});
});

/* istanbul ignore if */

if (!module.parent) {
  const server = app.listen(process.env.PORT || 2345, '127.0.0.1', () => {
    const {address, port} = server.address();
    console.log('Server running at http://%s:%d/suggestions', address, port);
  });
}

export default app;
