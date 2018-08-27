const {createInterface} = require('readline');
const {createReadStream} = require('fs');

const tab = String.fromCharCode(0x9);

// This file has been kept as generic as possible. It could be used to load any csv or tsv
function dataLoader({
  delimiter = tab,
  lineHandler, // If we want to manipulate data once an entry has been read from the file
  path,
}) {
  return new Promise(resolve => {
    const columns = [];
    const entries = [];
    // readline is interesting here since it reads the file line by line, instead of loading the whole file in memory.
    // I know it's not important here since the dataset is rather small, but on much larger files where you might pipe
    // the results into another database like Redis or Elasticsearch it will make a huge difference.
    const lineReader = createInterface({
      input: createReadStream(path),
    });

    lineReader.on('line', line => {
      const parsedLine = line.split(delimiter);
      if(!columns.length) {
        // We're reading the first line, so the columns
        columns.push(...parsedLine);
      } else {
        // We're reading a entry
        let entryObj = {};
        columns.forEach((columnName, idx) => {
          entryObj[columnName] = parsedLine[idx];
        });
        
        if(lineHandler) entryObj = lineHandler(entryObj);
        entries.push(entryObj);
      }
    });

    lineReader.on('close', () => {
      resolve(entries);
    });
  });
}

module.exports = dataLoader;
