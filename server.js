/**
 * Handle HTTP server
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Start the http server and handle listening and error events
 * 
 * @param  {Object}  app express app
 * @return {Object} the server object
 */
module.exports = function(app) {
  
  var server = di.http.createServer(app);
  var port = process.env.PORT || di.config.getServer().port;

  /**
   * Fired when the server is start listening
   */
  server.on('listening', function() {
    console.log('[*] Server listening on port', di.chalk.green(di.config.getServer().port));
  });

  /**
   * Fired when the server faces an error in listening on the port
   *
   * @param {Error} error error object
   */
  server.on('error', function(error) {

    if (error.syscall !== 'listen') {
      throw error;
    }

    // Handle specific listen errors with friendly messages
    switch (error.code) {
      case 'EACCES':
        console.error('[X] Port ' + port + ' requires elevated privileges');
        process.exit(1);
        break;
      case 'EADDRINUSE':
        console.error('[X] Port ' + port + ' is already in use');
        process.exit(1);
        break;
      default:
        throw error;
    }

  });

  /**
   * Start listening on the server's port and receiving requests
   */
  server.start = function() {

    server.listen(port);
    
  };

  return server;

};
