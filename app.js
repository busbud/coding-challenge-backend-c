require('dotenv').config();
const cluster = require('cluster');
const http = require('http');
http.globalAgent.maxSockets = Infinity;

if ((cluster.isMaster) && (process.env.NODE_ENV !== 'development')) {
  // Count the machine's CPUs
  const cpuCount = require('os').cpus().length;

  // Create a worker for each CPU
  for (var i = 0; i < cpuCount; i += 1) {
    cluster.fork();
  }
  // Listen for dying workers
  cluster.on('exit', function(worker) {
    // Replace the dead worker, we're not sentimental
    console.log('Worker %d died :(', worker.id);
    cluster.fork();
  });

// Code to run if we're in a worker process
} else {
  const express = require('express');
  const app = express();
  const cors = require('cors');


  // Allow cors from everywhere
  app.use(cors());

  const suggestionsRoutes = require('./api/routes/suggestions');

  app.use('/suggestions', suggestionsRoutes);

  const port = process.env.PORT || 2345;
  // const host = process.env.HOST || 'localhost';

  app.use((req, res, next) => {
    const error = new Error('These aren\'t the droids you\'re looking for.');
    error.status = 404;
    next(error);
  });

  app.use((error, req, res, next) => {
    res.status(error.status || 500);
    res.json({
      error: {
        message: error.message,
        status: (error.status || 500)
      }
    });
  });

  const workerMessage = (process.env.NODE_ENV !== 'development') ? `worker id =${cluster.worker.id}` : '';

  module.exports = app.listen(port, () => {
    console.log(`Server is running at ${port} ${workerMessage}`);
  });
}
