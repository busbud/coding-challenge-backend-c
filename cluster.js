var cluster = require('cluster');
var log     = require('debug')('cluster');

/**************************
 Cluster - Master Process
 **************************/

if (cluster.isMaster) {

    var cpuCount = require('os').cpus().length;
    for (var i = 0; i < cpuCount; i += 1) {
        log('Cluster Master :: Spawning Worker Process');
        cluster.fork();
    }

    // Auto-respawn dying worker processes.
    cluster.on('exit', function (worker) {
        log('Cluster Worker [' + worker.id + '] :: Died. Respawning...');
        cluster.fork();
    });

/**************************
  Cluster - Worker Process
***************************/

} else {

    var app = require(__dirname + '/app');
    app.initialize(function(error, response) {
        if (!error) {
            log('Cluster Worker [' + cluster.worker.id + '] :: ' + response);
        } else {
            log('Cluster Worker [' + cluster.worker.id + '] :: ' + error);
        }

    });

}

