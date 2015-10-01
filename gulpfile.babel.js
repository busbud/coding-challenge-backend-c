import gulp from "gulp";
import stylus from "gulp-stylus";
import util from "gulp-util";
import notify from "gulp-notify";
import watchify from "watchify";
import merge from "merge-stream";
import browserify from "browserify";
import babelify from "babelify";
import source from "vinyl-source-stream";
import buffer from "vinyl-buffer";
import uglify from "gulp-uglify";
import sourcemaps from "gulp-sourcemaps";
import $ from "jquery";

const scriptsGlob = "src/**/*.es6"
const extensions = [".js", ".es6"];
const destination = "./public/js/";
const paths = ['./node_modules', './src/js/lib'];

const files = [
  {
    input: ["./src/js/frontend/client.js"],
    output: "client.js",
    extensions: extensions,
    destination: destination
  }
];

const stylusFiles = [
  {
    name: "main",
    src: ["./src/styl/main.styl"],
    globs: ["./src/styl/main.styl", "./src/styl/**/*.styl", "./src/styl/**/*.css"],
    destination: "./build/css/"
  }
];

function createBundle(options) {
  const bOpts = {
    debug: true,
    extensions: options.extensions,
    paths: paths,
    entries: options.input,
    cache: {}, packageCache: {}, fullPaths: true
  };

  let bundler = (global.isWatching ?
      watchify(browserify(bOpts)) :
      browserify(bOpts)
  ).add(require.resolve("babel/polyfill"))
    .transform(babelify.configure({
      extensions: options.extensions, compact: false, ignore: /(bower_components)|(node_modules)/
    }));

  if (global.isWatching) {
    bundler.on("update", () => {
      const start = Date.now();
      executeBundle(bundler, options, start);
    });
  }

  executeBundle(bundler, options, Date.now());
};

function createBundles(bundles) {
  bundles.forEach((bundle) => {
    createBundle({
      input: bundle.input,
      output: bundle.output,
      extensions: bundle.extensions,
      destination: bundle.destination
    });
  });

}

const global = {
  isWatching: false
};

gulp.task("watch", () => {
  global.isWatching = true;
  createBundles(files);
  watchStyluses(stylusFiles);
});

gulp.task("compile", () => {
  stylusFiles.forEach((file) => {
    executeStylus(file.name, file.src, Date.now()) ;
  });
  createBundles(files);
});


function executeBundle(bundler, options, startTime) {
  console.log("Detected filesystem change. Recompiling...");
  return bundler.bundle()
    .on("error", util.log.bind(util, "Browserify Error"))
    .pipe(source(options.output))
    .pipe(buffer())
    .pipe(uglify({ mangle: false }))
    .pipe(sourcemaps.write(options.destination))
    .pipe(gulp.dest(options.destination))
    .pipe(notify(`Build of ${options.output} finished`))
}

function watchStyluses(files) {
  files.forEach((file) => {
    watchCss(file.name, file.globs, file.src);
    executeStylus(file.name, file.src, Date.now()) ;
  });
}

function executeStylus(name, src, startTime) {
  return gulp.src(src)
    .pipe(sourcemaps.init())
    .pipe(stylus({
      'include css': true,
      'compress': true
    }))
    .pipe(sourcemaps.write())
    .pipe(gulp.dest("./public/css/"))
    .pipe(notify(`Stylus build of ${name} finished`));
}

function watchCss(name, watch, src) {
  gulp.watch(watch, () => {
    console.log("Rebuilding css...");
    const startTime = Date.now();
    executeStylus(name, src, startTime);
  });
}

gulp.task("default", ["stylus"]);
