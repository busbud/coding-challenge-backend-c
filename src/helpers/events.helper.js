/**
 * Required Modules.
 */
const process = require('process');

// When an exception is not uncaught by the server.
const onException = (error) => console.error({ message: error });

// When the process receives the kill signal.
const onProcessKill = (server) => {
  console.info('Service termination signal received');

  setTimeout(() => {
    console.info('Finishing server...');
    server.close(() => process.exit(0));
  }, 100);
};

/**
 * Export the helper methods.
 */
module.exports = {
  onException,
  onProcessKill,
};
