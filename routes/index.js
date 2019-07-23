// General Requirements
const app = require('express')();
// Application Code
const suggestionsRoutes = require('./suggestions');
const streamRoutes = require('./stream');

// load suggestion routes
app.use('/suggestions', suggestionsRoutes);
app.use('/stream', streamRoutes);

// catch all routes
app.all('*', function(req, res) {
  res.status(404).send({ msg: 'Not Found' });
});

module.exports = app;
