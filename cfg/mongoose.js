var mongoose = require('mongoose');

var dbURI =
  process.env.MONGODB_URI ||
  'mongodb://localhost/geolocationapi';

mongoose.connect(dbURI, function(err) {
  if (err) {
    console.error('Database Connection Error: ' + err.message);
    throw err;
  }
});

mongoose.connection.once('connected', function() {
  console.log('Database Connection established to: ' + dbURI);
});

mongoose.connection.on('error', function(err) {
  console.error('Database Connection Error: ' + err.message +
    '. Attempting to reconnect...');
});

module.exports = mongoose.connection;
