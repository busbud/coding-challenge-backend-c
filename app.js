const express = require('express');
const app = express();
const port = process.env.PORT || 3104;
const Suggestions = require('./Services/Suggestions/router');
const expressLimiter = require('express-rate-limit');

const bruteForce = expressLimiter({
  windowMs: 1 * 60 * 1000, // 10 requests (max) per miniute per IP
  max: 10,
});

// app.use(bruteForce);
app.use('/suggestions', Suggestions);

app.listen(port, () => {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});

module.exports = app;
