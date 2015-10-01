const cluster = require('cluster');
const server = require('./server');
const parser = require('./parser');

if (cluster.isMaster) {
  var workers = require('os').cpus().length;

  console.info('Starting a cluster of ' + workers + '.');

  for (var i = 0; i < workers; i++) {
    cluster.fork();
  }

  cluster.on('online', (worker) => {
    console.log('Busbud suggestion server ' + worker.process.pid + ' has started.');
  });

  cluster.on('exit', (worker, code, signal) => {
    console.error('Worker ' + worker.process.pid + ' has exited.');
    console.error('-- code: ' + code + '\n-- signal: ' + signal);
    cluster.fork();
  });

} else {
  parser('data/cities_canada-usa.tsv', (err, data) => {
    if (!err) {
      server.cities = data;
      server.listen(process.env.PORT || 3000, () => {
        console.info('Server listening at ' + server.url);
      });
    } else {
      throw new Error('Error parsing cities file.');
    }
  });
}