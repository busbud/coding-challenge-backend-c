const express = require('express');
const app = express();
const port = process.env.PORT || 3000;
const routes = require('./routes');

app.get('/health-check', (req, res) => {
  res.status(200).send({
    success: 'true',
    message: 'unconditionally healthy'
  });
});

app.use('/suggestions', routes());

app.get('/*', (req, res) => {
   res.status(404).send('Not Found!');
});

app.listen(port, () => {
   console.log('Server is up!', port);
});

module.exports = app;