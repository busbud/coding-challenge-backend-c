'use strict';

import express from 'express';

const app = express();

app.get('/suggestions', (req, res) => {
  res.status(404);
  res.json({suggestions: []});
});

const server = app.listen(process.env.PORT || 2345, '127.0.0.1', () => {
  const {address, port} = server.address();
  console.log('Server running at http://%s:%d/suggestions', address, port);
});

export default app;
