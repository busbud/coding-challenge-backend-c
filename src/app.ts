import {startServer} from "./server/server";
// eslint-disable-next-line @typescript-eslint/no-var-requires
const cluster = require('cluster');
// eslint-disable-next-line @typescript-eslint/no-var-requires
const numCPUs = require('os').cpus().length;
// eslint-disable-next-line @typescript-eslint/no-var-requires
const process = require('node:process');

if (cluster.isPrimary) {
    for (let i = 0; i < numCPUs; i++) {
        cluster.fork();
    }

    cluster.on('exit', (worker: { process: { pid: any } }, code: any, signal: any) => {
      console.log(`worker ${worker.process.pid} died`);
      cluster.fork();
    });
} else {
    startServer().catch((e) => {
        console.error("Server failed to start", e)
    });
}
