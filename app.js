var startServer = require('./server');
var port = process.env.PORT || 2345;


startServer(port);


console.log('Server running at http://127.0.0.1:%d/suggestions', port);