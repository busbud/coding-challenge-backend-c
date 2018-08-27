const express = require('express');
const { routes } = require('./routes');

const port = process.env.PORT || 3000;
const app = express();

app.use('/', routes);

app.listen(port, () => {
  console.log(`Express app started on port ${port}`);
});

module.exports = { app };
