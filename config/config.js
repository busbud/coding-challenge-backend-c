'use strict';
/**
 * config.js
 * ------------------------------
 * Taken from the MEAN stack
 */

/**
 * Module dependencies.
 */
var _ = require('lodash'),
    glob = require('glob'),
    fs = require('fs');

/**
 * Resolve environment configuration by extending each env configuration file,
 * and lastly merge/override that with any local repository configuration that exists
 * in local.js
 */
var resolvingConfig = function() {
    var conf = {};

    conf = _.extend(
        require('./env/all'),
        require('./env/' + process.env.NODE_ENV) || {}
    );

    return _.merge(conf, (fs.existsSync('./config/env/local.js') && require('./env/local.js')) || {});
};

/**
 * Load app configurations
 */
module.exports = resolvingConfig();

/**
 * Get files by glob patterns
 */
module.exports.getGlobbedFiles = function(globPatterns, removeRoot) {
    // For context switching
    var _this = this;

    // URL paths regex
    var urlRegex = new RegExp('^(?:[a-z]+:)?\/\/', 'i');

    // The output array
    var output = [];

    // If glob pattern is array so we use each pattern in a recursive way, otherwise we use glob
    if (_.isArray(globPatterns)) {
        globPatterns.forEach(function(globPattern) {
            output = _.union(output, _this.getGlobbedFiles(globPattern, removeRoot));
        });
    } else if (_.isString(globPatterns)) {
        if (urlRegex.test(globPatterns)) {
            output.push(globPatterns);
        } else {
            glob(globPatterns, {
                sync: true
            }, function(err, files) {
                if (removeRoot) {
                    files = files.map(function(file) {
                        return file.replace(removeRoot, '');
                    });
                }

                output = _.union(output, files);
            });
        }
    }

    return output;
};
