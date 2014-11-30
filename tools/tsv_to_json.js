#!/usr/bin/env node


// command line tool for convert tsv file to json

var program = require('commander'),
    http = require('http'),
    fs = require('fs'),
    path = require('path'),
    tsv = require("node-tsv-json");

program
  .version('0.0.1')
  .option('-s, --source <path>', 'file to convert')
  .option('-d, --destination <path>', 'file to output json')
  .parse(process.argv);

if (typeof(program.source) === 'string' && typeof(program.destination) === 'string' ) {
  if (fs.existsSync(program.source)) {
    tsvToJson(program.source, program.destination);
  } else{
    console.error('Source File: ' + sourceFile+ ' not found.');
    process.exit(1);
  }
} else {
  program.help();
}


function tsvToJson(src,output) {
  return tsv({ input: src,
               output: output,
               parseRows: false }, function(err, result){
                if(err) {
                  console.error(err);
                  process.exit(1);                    
                }
               });
}