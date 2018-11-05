
/**
 * Native modules
 */

const cluster = require('cluster')

/**
 * Worker
 */

const worker = require('./src/worker')

/**
 * Config
 */

const config = require('./config')



if (cluster.isMaster) {

	for (let i = 0; i < config.options.cpu; i++) {

		cluster.fork()

	}

	cluster.on('exit', (worker, code, signal) => cluster.fork())

} else {

	worker()

}


