var http = require('http');
var url = require('url');
var cluster = require('cluster');
var os = require('os');
var port = process.env.PORT || 2345;
var getCityScores = require('./dist/utils/compute-score').getCityScores;

function startServer() {
    return http.createServer(function (req, res) {
        console.log(`Received ${req.method} request on ${req.url} on process ${process.pid}`);
        //Only allow GETs
        if (req.method.toUpperCase() !== 'GET') {
            res.writeHead(401, { 'Content-Type': 'text/plain' });
            res.end(`${req.method} Not Allowed`);

        }
        else if (req.url.indexOf('/suggestions') === 0) {
            try {
                const params = url.parse(req.url, true).query;
                const searchTerm = params['q'];
                const longitude = params['longitude'];
                const latitude = params['latitude'];
                const scores = getCityScores(searchTerm, latitude, longitude);
                if (scores.length === 0) {
                    res.writeHead(404, { 'Content-Type': 'text/plain' });
                }
                else {
                    res.writeHead(200, { 'Content-Type': 'application/json' });
                }
                res.end(JSON.stringify({
                    suggestions: scores
                }));
            } catch (e) {
                res.writeHead(500, { 'Content-Type': 'text/plain' });
                console.log(e);
                res.end("Internal Error");
            }

        } else {
            res.writeHead(404, { 'Content-Type': 'text/plain' });
            res.end();
        }
    }).listen(port, '0.0.0.0');
}

/**
 * Clusters are introduced to leverage all CPUs of the host machine, to help balance load between multiple requests
 * Cluster use from https://www.sitepoint.com/how-to-create-a-node-js-cluster-for-speeding-up-your-apps/*/
function getClusteredServer() {
    if (cluster.isMaster && process.env.NODE_ENV !== 'test') {
        var numWorkers = os.cpus().length;
        console.log(`Optimizing to use ${numWorkers} workers...`);

        for (var i = 0; i < numWorkers; i++) {
            cluster.fork();
        }

        cluster.on('online', function (worker) {
            console.log('Worker ' + worker.process.pid + ' is online');
        });

        cluster.on('exit', function (worker, code, signal) {
            console.log('Worker ' + worker.process.pid + ' died with code: ' + code + ', and signal: ' + signal);
            console.log('Starting a new worker');
            cluster.fork();
        });
    } else {
        return startServer();
    }
}

module.exports = getClusteredServer();

console.log('Server running at http://0.0.0.0:%d/suggestions on process %d', port, process.pid);
