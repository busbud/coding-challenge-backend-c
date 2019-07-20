const log4js = require('log4js');
const dataImporter = require("./dataImporter")
const logger = log4js.getLogger();
logger.level = 'debug';

let data  = [];

async function importData(path, lineDelimiter){
  data = await dataImporter(path, 'utf8', true, lineDelimiter,function(unprocessedLine, lineIndex){
    return unprocessedLine;
  });
  console.log(data[0]);
  logger.info(`${data.length} data rows have been imported`);
}

module.exports = {
  importData: importData,
  getData: function(){ return data},
};