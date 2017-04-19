'use strict'

global.__basedir 	= __dirname;
global.MODELS_DIR 	= "models";
global.ROUTES_DIR 	= "routes";

/**
 * Module Dependencies
 */
const 
	config			= require('./config'),
	toobusy 		= require('toobusy-js'),
	restify			= require('restify'),
	bunyan			= require('bunyan'),
	winston			= require('winston'),
	bunyanWinston	= require('bunyan-winston-adapter'),
	mongoose		= require('mongoose');

/**
 * Logging
 */
global.log = new winston.Logger({
	transports: [
		new winston.transports.Console({
			level: 'info',
			timestamp: () => {
				return new Date().toString()
			},
			json: true
		})
	]
})

/**
 * Initialize Server
 */
global.server = restify.createServer({
    name    : config.name,
    version : config.version,
    log     : bunyanWinston.createAdapter(log),
})

/**
 * Middlewares
 */

// The first middleware to register. It blocks requests before spending any time 
// on them when the server is too busy.
server.use(function(req, res, next) {
	if (toobusy()) {
		// If node process is too busy, send 503 - Service Unavailable.
		res.send(503, "Too busy right now.");
	} else {
		next();	
	}
});

// Accept header parsing
server.use(restify.acceptParser(server.acceptable))

// HTTP querystring parsing (in req.query and merged in req.params)
server.use(restify.queryParser({ mapParams: true }))

// Request body parsing. Commented because not useful for 'suggestions' endpoint 
// server.use(restify.bodyParser());


/**
 * Error Handling
 */
server.on('uncaughtException', (req, res, route, err) => {
	log.error(err.stack);
	res.send(err);
});


server.listen(config.port, function() {
	
	// Log server startup
	log.info('%s listening at %s', server.name, server.url);
	
	// In case of database connection error, log and exit.
	mongoose.connection.on('error', function(err) {
		log.error('Mongoose default connection error: ' + err);
		process.exit(1);
	});

	mongoose.connection.on('open', function(err) {
		// Log any error related to database connection, then exit.
		if (err) {
			log.error('Mongoose default connection error: ' + err);
			process.exit(1);
		}

		// Log succesful database connection.
		log.info(
			'%s v%s ready to accept connections on port %s in %s environment.',
			server.name,
			config.version,
			config.port,
			config.env
		);

		// Make routes available.
		require('./routes');
	});

	// Make the database connection available in the app
	global.db = mongoose.connect(config.db.uri);
});

// SIGINT handler to exit the process gracefully.
process.on('SIGINT', function() {
	server.close();
	// calling .shutdown allows your process to exit normally 
	toobusy.shutdown();
	process.exit();
});

module.exports = server;
