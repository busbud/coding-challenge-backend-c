var fs = require('fs'),
    path = require('path')
    logger = require('./logger'),
    __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; };

module.exports = (function() {
  'use strict';
  function LoadInMemory(fileName, modelName) {
    this.fileName = fileName;
    this.modelName = modelName;

    this.buf = '';
    this.columnHeaders = null;
    this.datas = [];
    this.log = logger('LoadInMemory');
    this.pump = __bind(this.pump, this);
    this.memorize = __bind(this.memorize, this);
    this.jsonize = __bind(this.jsonize, this);
    this.loadFromJson = __bind(this.loadFromJson, this);
    this.loadFromTsv = __bind(this.loadFromTsv, this);
  }

  LoadInMemory.prototype.pump = function() {
    var pos;
    var items = [];
    while ((pos = this.buf.indexOf('\n')) >= 0) { // keep going while there's a newline somewhere in the buffer
        if (pos == 0) { // if there's more than one newline in a row, the buffer will now start with a newline
            this.buf = this.buf.slice(1); // discard it
            continue; // so that the next iteration will start with data
        }

        if (!this.columnHeaders) {
          this.setHeader(this.buf.slice(0,pos));
        } else {
          var i = this.jsonize(this.buf.slice(0,pos));
          if(i != null) items.push(i);
        }

        this.buf = this.buf.slice(pos+1); // and slice the processed data off the buffer
    }
    this.memorize(items);
  };


  LoadInMemory.prototype.setHeader = function(line) {
    line = line.split('\n');
    this.columnHeaders = line.shift().split('\t');
  }

  LoadInMemory.prototype.associateHeaderValues = function(data) {
    var r = {};
    this.columnHeaders.forEach(function(value, index){
      if(typeof(data[index]) != 'undefined') r[value] = data[index].toLowerCase();
    });
    return r;
  }

  LoadInMemory.prototype.jsonize = function(line) {
    var item = null;
    if (line[line.length-1] == '\r') line = line.substr(0,line.length-1);
    if (line.length > 0) { // Only complete line to parse
      item = this.associateHeaderValues(line.split('\t'));
    }
    return item;
  };

  LoadInMemory.prototype.memorize = function(items) {
    var self = this;
    items.forEach(function(item, index){
      self.datas.push(item);
    });
  };

  LoadInMemory.prototype.loadFromJson = function(callback) {
    try {
      var items = require(this.fileName);
      this.memorize(items);

      self.log('Loading %s cities created.', self.datas.length);
      callback(this.datas);
    } catch(e){

      self.log('Loading error %s.', e);
      callback(null,e);
    }
  };

  LoadInMemory.prototype.loadFromTsv = function(callback) {
    var self = this;
    var stream = fs.createReadStream(this.fileName);
    stream.setEncoding('utf-8');
    stream.on('data', function(chunk) {
      self.buf += chunk.toString();
      self.pump();
      return;
    });
    stream.on('end', function() {
      self.log("Finished reading chunks!");

      self.log('Loading data. %s cities created.', self.datas.length);
      return callback(self.datas);
    });
  };

  LoadInMemory.prototype.load = function(callback) {
    this.log('Stream file: %s', this.fileName);
    callback = callback || function(){};
    if(path.extname(this.fileName) === '.json'){
      this.loadFromJson(callback);
    } else if(path.extname(this.fileName) === '.tsv'){
      this.loadFromTsv(callback);
    } else {
      self.log("Please specify json or tsv file");
    }
  };

  return LoadInMemory;

})();