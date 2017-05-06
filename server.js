'use strict';
/**
 * server.js
 * ------------------------------
 */

// NODE_ENV configs
require('./config/env');

/**
 * Module dependencies.
 */
 var init = require('./config/init')(),
    config = require('./config/config'),
    mongoose = require('mongoose'),
    chalk = require('chalk'),
    path = require('path');

// Bootstrap db connection
var db = mongoose.connect(config.db.uri, config.db.options, function(err) {
    if (err) {
        console.error(chalk.red('Could not connect to MongoDB!'));
        console.log(chalk.red(err));
    }
});
mongoose.connection.on('error', function(err) {
        console.error(chalk.red('MongoDB connection error: ' + err));
        process.exit(-1);
    }
);

// Globbing model files
config.getGlobbedFiles('./app/models/**/*.js').forEach(function(modelPath) {
    require(path.resolve(modelPath));
});

// Seeding data
require('./seed')();

var app = require('express')();
var routes = require('./app/routes/index.server.routes.js')(app);
var server = app.listen(config.port, function () {
    // Logging initialization
	console.log();
	console.log('-----------------------------------------------------------------');
	console.log(chalk.yellow(config.app.title + ' application started'));
	console.log();
	console.log(chalk.green('Environment:\t\t') + chalk.magenta(process.env.NODE_ENV));
	console.log(chalk.green('Port:\t\t\t') + chalk.magenta(config.port));
	console.log(chalk.green('Database:\t\t') + chalk.magenta(config.db.uri));
	if (process.env.NODE_ENV === 'secure') {
	    console.log(chalk.green('HTTPs:\t\t\t') + chalk.magenta('on'));
	}
	console.log('-----------------------------------------------------------------');
	console.log();
});

// Expose app
module.exports = app;

process.on('uncaughtException', function(e) {
   console.log('An error has occured. error is: %s and stack trace is: %s', e, e.stack);
   console.log("Process will restart now.");
   process.exit(1);
})

