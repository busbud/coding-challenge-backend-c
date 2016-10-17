var cluster = require('cluster');
var cache = require('cluster-node-cache')(cluster);
var numCPUs = require('os').cpus().length;
var App = require('./app');
var server = new App(false,cache);
var port = process.env.PORT || 2345;

module.exports = function(){
  if(cluster.isMaster) {
      for(var i=0; i < numCPUs; i++){
        worker = cluster.fork();
      }
      cluster.on('exit', function(worker, code, signal) {
          console.log('Worker ' + worker.process.pid + ' died with code: ' + code + ', and signal: ' + signal);
          //cluster.fork();
      });
  } else if(cluster.isWorker){
      // each child process gets to listen on the same port
      server.listen(port, '0.0.0.0');
      console.log('Server running at http://127.0.0.1:%d/suggestions', port);
      console.log('Process ' + process.pid + ' is listening to all incoming requests');
  }
}

module.exports();
