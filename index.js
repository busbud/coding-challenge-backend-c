require('babel/register');

var port = process.env.PORT || 8080;

var app = require('./app');

app.listen(port);
console.log('Server listening on port ' + port);
