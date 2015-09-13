var cool        = require('cool-ascii-faces');
var express     = require('express');
var suggestions = require('./routes/suggestions');

/**
 * Setting up the app
 */
const app = express();

/**
 * Setting up heroku
 */
app.get('/cool', function(request, response) {
  response.send(cool());
});

/**
 * Setting up the UI of the app
 */
app.use(express.static('public'));

/**
 * Setting up the API of the app
 */
app.use(suggestions);

/**
 * Starting server
 * @type {http.Server}
 */
var server = app.listen(3000, function () {
  console.log('triggered app');
  var host = server.address().address;
  var port = server.address().port;

  console.log('Suggestions app listening at http://%s:%s', host, port);
});

module.exports = server