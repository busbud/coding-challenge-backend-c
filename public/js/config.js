requirejs.config({
    baseUrl: '../js',
    shim: {
        underscore: {
            exports: '_'
        },
        jquery: {
            exports: '$'
        },
        backbone: {
            exports: 'backbone'
        },
        mustache: {
            exports: 'mustache'
        }
    },
    paths: {
        app: './app',
        backbone: 'bower_components/backbone/backbone',
        underscore: 'bower_components/underscore/underscore',
        jquery: 'bower_components/jquery/dist/jquery',
        mustache: 'bower_components/mustache/mustache',
        text: 'bower_components/text/text',
        bootstrap: 'bower_components/bootstrap/dist/js/bootstrap'
    },
    packages: [

    ]
});

define(function (require) {
    var app = require('app');
    app();
});
