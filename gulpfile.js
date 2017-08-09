var gulp = require("gulp");

var livereload 	= require('gulp-livereload');
var server     = require( 'gulp-develop-server' );
var plumber 	= require("gulp-plumber");


var options = {
    path: './app.js'
};

var serverFiles = [
    './app.js',
];

gulp.task( 'server:start', function() {
    server.listen( options, livereload.listen );
});

gulp.task( 'default', [ 'server:start' ], function() {

    function restart( file ) {
        server.changed( function( error ) {
            if( ! error ) livereload.changed( file.path );
            console.log('restarted!');
        });
    }

    gulp.watch( serverFiles ).on( 'change', restart );
});