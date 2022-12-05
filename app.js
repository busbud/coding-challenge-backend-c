const express = require('express');
const rateLimiter = require('./middlewares/RateLimiter');
const app = express();
const port = process.env.PORT || 3000;
const routes = require('./routes');

// adding a rate limiter to handle large traffic or network attacks
app.use(rateLimiter);

// this is just a health check used for deployment
// we can check if the service is up by pinging this route
app.get('/health-check', (req, res) => {
  res.status(200).send({
    success: 'true',
    message: 'unconditionally healthy'
  });
});

app.use('/suggestions', routes());

// handle unkown routes
app.get('/*', (req, res) => {
  res.status(404).send({ message: 'Route not found' });
});

app.listen(port, () => {
   console.log('Server is up!', port);
});

module.exports = app;