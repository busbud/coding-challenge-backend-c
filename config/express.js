'use strict';

/**
 * Module dependencies.
 */
 var express = require('express'),
 bodyParser = require('body-parser'),
 compression = require('compression'),
 methodOverride = require('method-override'),
 favicon = require('serve-favicon'),
 morgan = require('morgan'),
 multer = require('multer');

 module.exports = function(app, db) {
    app.set('showStackError', true);

    // cache=memory
    app.locals.cache = 'memory';

    // Should be placed before express.static
    // To ensure that all assets and data are compressed (utilize bandwidth)
    app.use(compression({
        filter: function(req, res) {
            return (/json|text|javascript|css/).test(res.getHeader('Content-Type'));
        },
        // Levels are specified in a range of 0 to 9, where-as 0 is
        // no compression and 9 is best compression, but slowest
        level: 9
    }));

    app.use(morgan('dev'));

    // Enable jsonp
    app.enable('jsonp callback');

    // The cookieParser should be above session
    //app.use(cookieParser());

    // Request body parsing middleware should be above methodOverride
    app.use(bodyParser.urlencoded({ extended: true }));
    app.use(bodyParser.json());
    app.use(methodOverride());
    app.use(multer());

    // Setting the fav icon and static folder
    app.use(express.static('../public'));
    
};
