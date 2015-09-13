#!/usr/bin/env node

"use strict";

var colors = require('colors');
var procs = require('../lib/procs');
var json = require('../package');
var path = require('path');

var files = [];
var template;
var watch = false;

var args = process.argv.slice(2);

function help(err){
	console.warn('\n ' + ' PROCS -- Stupid simple Markdown documentation generator '.inverse.cyan + '\n');
	console.log(
		'  --help, -h     : usage information\n' +
		'  --version, -v  : prints the version number\n' +
		'  --file, -f     : file to compile\n' +
		'  --template, -t : template file\n' +
		'  --watch, -w    : watch the source files for changes\n');
	process.exit(err);
}

for (var i = 0; i < args.length; i++){

	switch (args[i]){
		case '--help':
		case '-h':
			help(0);
			break;
		case '--version':
		case '--v':
			console.log(json.version);
			process.exit(0);
			break;
		case '--file':
		case '-f':
			files.push(path.normalize(process.cwd() + '/' + args[++i]));
			break;
		case '--template':
		case '-t':
			template = path.normalize(process.cwd() + '/' + args[++i]);
			break;
		case '--watch':
		case '-w':
			watch = true;
			break;
	}

}

if (!template || !files.length) help(1);

files.forEach(function(file){
	procs(file, template, {watch: watch}, function(err){
		if (err) return console.warn((err + '').red);
		console.log((file + " has been compiled\n").green);
	});
});

