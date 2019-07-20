const fs = require('fs');
const readline = require('readline'); //Interface to read data from Readable Stream one line at a time



function dataImporter(path, encoding, headers, lineDelimiter, lineProcessor) {
  return new Promise(resolve => {
    // setting defaults
    if (encoding === undefined) { encoding = "utf8"; }
    if (headers === undefined) { headers = false; }
    if(lineDelimiter === undefined){ throw("Must set a line delimiter"); }


    var lineIndex= 0;
    const fileStream = fs.createReadStream(path, {encoding: encoding});
    const lineReader = readline.createInterface({
      input: fileStream,
      crlfDelay: Infinity
    });
    // Note: we use the crlfDelay option to recognize all instances of carriage return &
    // Line feed ('\r\n') in the data file as a single line break.
    let columns = [];
    let data = [];
    lineReader.on('line', (line) => {
      if(lineProcessor){
        line = lineProcessor(line,lineIndex);
      }
      line = line.split(lineDelimiter);
      if(headers && (lineIndex == 0)){
        columns = line;
      } else {
        let dataLine = null;
        if(headers){
          dataLine = {};
          columns.forEach((column, columnIndex) => {
            dataLine[column] = line[columnIndex];
          });
        } else {
          dataLine = line;
        }
        data.push(dataLine);
      }
      lineIndex++;
    });
    lineReader.on('close', () => {
      resolve(data);
    });
  });
};

module.exports = dataImporter;