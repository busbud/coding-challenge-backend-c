"use strict";

var fs = require('fs');
var path = require('path');
var rs = require('robotskirt');
var hljs = require('highlight.js');

function build(file, layout, callback){

	var renderer = new rs.HtmlRenderer();

	renderer.blockcode = function(code, lang){
		if (!lang) return '<pre><code>' + code + '</code></pre>\n';
		if (lang == 'js') lang = 'javascript';
		else if (lang == 'html') lang = 'xml';
		else if (lang == 'shell') lang = 'bash';
		code = hljs.highlight(lang, code).value.trim();
		return '<pre><code class="' + lang + '">' + code + '</code></pre>\n';
	};

	var sidebar = '';
	renderer.header = function(text, level){
		if (level <= 2){
			sidebar += '<a href="#' + text + '"' + (level == 1 ? ' class="top"' : '') + '>' + text + '</a>\n';
			text = '<a href="#' + text + '" name="' + text + '">' + text + '</a>';
		}
		return '<h' + level + '>' + text + '</h' + level + '>\n';
	};

	var parser = new rs.Markdown(renderer, [rs.EXT_FENCED_CODE]);

	var md = fs.readFileSync(file).toString();
	var html = parser.render(md);
	html = layout.replace('{content}', html);
	html = html.replace('{sidebar}', sidebar);

	fs.writeFile(file.slice(0, -3) + '.html', html, callback);

}

function procs(file, template, options, callback){

	if (!options) options = {};

	var layout = fs.readFileSync(template).toString();
	var basename = path.basename(file);
	var extension = path.extname(file);

	if (extension != '.md'){
		return callback(new Error("File doesn't have the .md extension"));
	}

	build(file, layout, callback);

	if (options.watch) fs.watchFile(file, {interval: 10}, function(oldStat, newStat){
		if (newStat.mtime.getTime() === oldStat.mtime.getTime()) return;
		build(file, layout, callback);
	});

}

module.exports = procs;
