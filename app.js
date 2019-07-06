
var express = require('express'),
  app = express(),
  port = process.env.PORT || 2345,
  bodyParser = require('body-parser');

app.use(bodyParser.urlencoded({ extended: true }));
app.use(bodyParser.json());
var routes = require('./api/routes/suggestionRoute');
routes(app);

module.exports = app.listen(port);
console.log('Server running at http://127.0.0.1:%d/suggestions', port);