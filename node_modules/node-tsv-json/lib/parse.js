var fs = require('fs'),
    byline = require('byline');

var processTsv = {}

module.exports = processTsv;


processTsv.processStream = function(source, cb) {
  this.separator = '\t';
  this.record = []

  var lineCount = 0;
  var that = this;
  var stream = fs.createReadStream(source, { encoding: 'utf8' })

  // Process data by line as long as there is a processor
  input = byline(stream).on('data', function (line) {
    
    if (lineCount == 0)
      that.header(line);
    else if (lineCount > 0) 
      that.recordData(line);
    else
      stream.destroy();
    lineCount++;
  })
  .on('end', function () {
    // done
    cb(that.fieldName, that.record)
  });
}

processTsv.header = function(line) {
  // Split line into field names
  fieldName = line.split(this.separator);
  this.fieldName = fieldName;
}

processTsv.recordData = function(line) {
  // Split line into fields
  var fields = line.split(this.separator);
  this.record.push(fields);
}
