![wrapup](http://github.com/kamicane/wrapup/raw/master/assets/wrapup.png)

## WrapUp?

 * WrapUp compiles CommonJS 1.0 modules for the browser.
 * WrapUp does not try to have a working `require` implementation for the browser, infact the loader WrapUp uses is [incredibly simple](https://github.com/kamicane/wrapup/blob/master/includes/wrapper.js).
 * WrapUp ignores duplicates that may be present when using npm to install packages.
 * WrapUp supports building multiple versions of the same package.
 * WrapUp supports circular module dependencies.
 * WrapUp can watch source files for changes and rebuild automatically.

[![Build Status](https://secure.travis-ci.org/mootools/wrapup.png)](https://travis-ci.org/mootools/wrapup)

## Installation

WrapUp is installed via npm:

```
npm install wrapup -g
```

After that, you will have access to `wrup` in your cli.

```
wrup --help
```

You can also install locally:

```
npm install wrapup
```

And require WrapUp in your node javascripts:

```js
var wrup = require("wrapup")()
```

## Usage

In a nutshell, you tell WrapUp you require `something`, it calculates
dependencies for `something` using static analysis, and compiles a single
JavaScript file that only exposes that `something` you required. `require`
paths inside modules are replaced with unique identifiers for brevity, and you
will only be able to access directly that `something` you required, never
dependencies (unless specifically required).

### require()

The main WrapUp method is `require(namespace, module)`.

It resolves a module using node's own modules and packages logic, so for
instance, `wrup.require("colors")` would look in your `node_modules` folder for
a package named colors, then proceed to load its `main`. The namespace parameter
is optional, but it's used to expose the module to the browser. Without a
namespace, the module will be required without being assigned. A bit like doing
`var x = require(y)` vs `require(y)`.

#### cli

```
wrup --require colors colors --require someName ./path/to/otherModule --require someOtherPackage
```

#### js

```js
var wrup = require("wrapup")() // require + instantiate

wrup.require("colors", "colors")
    .require("someName", "./path/to/otherModule")
    .require("someOtherPackage")
    .options(/*...options...*/)
    .up(function(err, js){
        console.log(js)
    })
```

the above would let you access colors and someName, while having
someOtherPackage simply required without being assigned to any variable. The
ouput code assigning variables would look like this:

```js
window.colors = require("colors")
window.someName = require("someName")
require("someOtherPackage")
```

### watch

WrapUp supports watching source files and rebuilds automatically whenever one of
these changes.

cli: `--watch`

Instead of using the `.up()` method, the `.watch()` method is used.

```javascript
var wrup = require("wrapup")() // require + instantiate
wrup.require("y", "./moduley.js").watch()

wrup.on("data", function(js){
    fs.writeFile("path/to/wherever", js)
})

wrup.on("change", function(file){
    console.log(file + " changed.")
})
```

In the above example, whenever module y and any module required by module y
changes, .up() is called again. The `data` event is fired whenever WrapUp
builds, either be a direct .up() call or an .up() call triggered by a changed
file. The `change` event is fired whenever `watch` is set to true and one of the
source files changes.

### options

Set some options for the output.

```js
wrup.options({
    globalize: "MyNamespace"
})
```

 - `globalize` define the global scope where named modules are attached to. Defaults to window.
 - `compress` if set to true, will compress the resulting javascript file using uglify-js. Defaults to false.
 - `output` only available in the cli, used to specify an output file. defaults to stdout.
 - `sourcemap` (cli: `--source-map`) Specify an output file where to generate source map.
 - `sourcemapURL` (cli: `--source-map-url`) `//@ sourceMappingURL` value, URL to the saved sourcemap file.
 - `sourcemapRoot` (cli: `--source-map-root`) The path to the original source to be included in the source map.
 - `sourcemapIn` (cli: `--in-source-map`) Input source map, useful if you're compressing JS that was generated from some other original code.

#### cli

```
wrup --require ... --globalize MyNameSpace --compress --output path/to/file.js --watch
```

#### js

```javascript
wrup.require(/*...*/)
    .require(/*...*/)
    .options({
        globalize: "MyNameSpace",
        compress: true,
        sourcemap: "./somefile.map"
    }).on("data", function(js){
        fs.writeFile("./somefile.js", js)
    }).up()
```

### Using Source Maps

WrapUp utilizes UglifyJS internally to create source-maps, and has the same
options, `--source-map`, `--source-map-root` and `--in-source-map`.

Once the `.map` file is created, the page with the JavaScript can be opened. It
is important that the original files are accessible through http too. For example
when using `--require ./test/a --source-map test.map --source-map-root
http://foo.com/src` the file `http://foo.com/src/test/a.js` should be the
original JavaScript module.

### Using with Uglify-JS

The WrapUp output can be piped into UglifyJS if more compression options are
desired. For example using the `--define` option to set global definitions.

```
wrup -r ./main.js --source-map ./main.map \
     | uglify -d DEV=false --compress --mangle --output ./main.min.js \
              --source-map main.map --in-source-map main.map
```

### Stream and pipe

WrapUp implements Node [Stream](http://nodejs.org/api/stream.html#stream_readable_stream)
which means it is possible to pipe the WrapUp output to other writable streams,
like [fs.WriteStream](http://nodejs.org/api/fs.html#fs_fs_writestream),
process.stdout or
[http.ServerResponse](http://nodejs.org/api/http.html#http_class_http_serverresponse).

```js
http.createServer(function(req, res){
    var wrup = wrapup()
    wrup.require('prime')
    wrup.pipe(res)
    wrup.up()
})
```

### Examples

coming soon... :)
