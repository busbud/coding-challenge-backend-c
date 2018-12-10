const DEBUG = process.env.NODE_ENV === 'debug';
const CI    = process.env.CI === 'true';

var gulp = require('gulp');
var gp   = require('auto-plug')('gulp');

gulp.task('lint', function () {
  return gulp.src(['lib/*.js'])
    .pipe(gp.jshint())
    .pipe(gp.jshint.reporter(require('jshint-stylish')));
});

gulp.task('test', ['lint'], function () {
  return gulp.src(['test/*.test.js'], {read: false})
    .pipe(gp.spawnMocha({
      debugBrk: DEBUG,
      r: 'test/setup.js',
      R: CI ? 'spec' : 'nyan',
      istanbul: !DEBUG
    }));
});

gulp.task('bench', function () {
  return gulp.src(['bench/*.js'], {read: false})
    .pipe(gp.bench());
});

gulp.task('default', function () {
  gulp.start('test');
  gulp.watch('{lib,test}/*', ['test']);
});
