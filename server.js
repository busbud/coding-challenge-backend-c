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
    path = require('path'),
    mongoose = require('bluebird').promisifyAll(require('mongoose')),
    chalk = require('chalk');

// Bootstrap db connection
mongoose.Promise = global.Promise;
mongoose.connect(config.db.uri, config.db.options)
.then(() => {
    // Globbing mongoose model files
    config.getGlobbedFiles('./app/models/**/*.js').forEach(function(modelPath) {
        require(path.resolve(modelPath));
    });
    // Seeding DB data if needed
    return require('./seed')();
}).then(() => {
    var app = require('express')();
    var routes = require('./app/routes/index.server.routes.js')(app);
    app.listen(config.port, function () {
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
}).catch(function(err) {
    console.error(chalk.red(err.stack || err));
});

process.on('uncaughtException', function(e) {
   console.log('An error has occured. error is: %s and stack trace is: %s', e, e.stack);
   console.log("Process will exit now.");
   process.exit(1);
})

