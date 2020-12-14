var http = require('http');
var port = process.env.PORT || 2345;
var server_host = process.env.YOUR_HOST || '0.0.0.0';
var cluster = require('cluster');
var express = require('express');
var app = express();

var suggestions = require('./api/api-suggestion-v1');


var env = process.env.NODE_ENV;
// module.exports = http.createServer(function (req, res) {
//   res.writeHead(404, {'Content-Type': 'text/plain'});

//   if (req.url.indexOf('/suggestions') === 0) {
//     res.end(JSON.stringify({
//       suggestions: []
//     }));
//   } else {
//     res.end();
//   }
// })
console.log("env is", env)
if (cluster.isMaster && env === 'prod') {
  const numWorkers =  2

  console.log('Master cluster setting up ' + numWorkers + ' workers')

  for (let i = 0; i < numWorkers; i++)
    cluster.fork()

  cluster.on('online', (worker) => {
    const msg = 'Worker ' + worker.process.pid + ' is online'
    console.log(msg)
  })

  cluster.on('exit', (worker, code, signal) => {
    const msg = 'Worker ' + worker.process.pid + ' died with code: ' + code + ', and signal: ' + signal
    console.log(msg) 
    cluster.fork()
  })

  cluster.on('disconnect', (worker) => {
    const msg = 'The worker ' + worker.process.pid + ' has disconnected'
    console.log(msg);
    
  });

  cluster.on('fork', (worker) => {
    const msg = 'The worker ' + worker.process.pid + ' has been forked'
    console.log(msg);
    
  });

} else {
  app.server = http.createServer(app);

  app.server.listen(port, server_host);

  app.use('/suggestions', suggestions)

  console.log('Server running at ',server_host, port);
}

module.exports = app;