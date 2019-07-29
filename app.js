const db = require('./db');
const config = require('./config').get();
const logger = require('./logger')(config);
const server = require('./server/server');
const es = require('@elastic/elasticsearch');
const cluster = require('cluster');
const os = require('os');

const cpuCount = os.cpus().length;

// cluster module will allow us to have an instance of the server running for
// each cpu core in the current machine. Which will result in better performace
if (cluster.isMaster) {
  console.log(`creating ${cpuCount} workers.`);

  for(let i = 0; i < cpuCount; i++) {
    cluster.fork();
  }

  cluster.on('online', function(worker) {
    console.log(`Worker ${worker.process.pid} is online`);
  });

  cluster.on('exit', function(worker, code, signal) {
    console.log(`Worker ${worker.process.pid}  died with code: ${code} , and signal:  ${signal}`);
    console.log(`creating new worker`);
    cluster.fork();
  });

}
else {
  initialize();
}

/**
 * APP MODULE
 * This is the entrypoint of the api
 */
async function initialize() {
  let client;

  try {
    client = await db.getClient(config.db, es);
  }
  catch (err) {
    logger.error('could not init db');
    process.exit(1);
  }

  server.start(config, logger, client, db);
}


