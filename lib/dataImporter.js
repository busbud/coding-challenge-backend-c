//  Node Standard
const fs        = require('fs');
const util      = require('util');
//  General Libraries
const readline  = require('readline');


//  Define Error
function DataImporterError(msg) {
  var err = Error.call(this, msg);
  err.name = 'DataImporterError';
  return err;
}
util.inherits(DataImporterError, Error);


/**
 * Parses a data file
 * @param   {string}    path                      Data file attempting to import
 * @param   {string}    encoding                  Data file encoding type
 * @param   {boolean}   headers                   True if data file contains header
 * @param   {string}    line_delimiter            Character that delimates a line
 * @param   {function}  line_processor_callback   Function called to process a line of data, after the initial processing has been done
 * @param   {function}  line_filter_callback      Function called to filter out lines of data
 * @return  {Array}     results         Array contained the formated parsed lines
 */
async function dataImporter(path, encoding, headers, line_delimiter, line_processor_callback, line_filter_callback) {
  return new Promise((resolve, reject)=> {
    //  error checks
    if ((line_delimiter === undefined) || !line_delimiter.length) {
      throw(new DataImporterError('Must set a line delimiter'))
    }

    //  setting defaults
    if (encoding === undefined) {
      encoding = 'utf8';
    }
    if (headers === undefined) {
      headers = false;
    }

    var line_index = 0;
    const file_stream = fs.createReadStream(path, { encoding: encoding });
    //  Note: we use the crlfDelay option to recognize all instances of carriage return &
    //  Line feed ('\r\n') in the data file as a single line break.
    const line_reader = readline.createInterface({
      input: file_stream,
      crlfDelay: Infinity
    });
    let columns = [];
    var data = [];
    line_reader.on('line', (line) => {
      //  breakdown a line into it's parts
      line = line.split(line_delimiter);
      // verify if headers exsits and if at file's first line
      if (headers && (line_index === 0)) {
        // processing header
        columns = line;
      } else {
        // process a line of data
        let data_line = null;
        if (headers) {
          // store data as a map based on file's header
          data_line = {};
          columns.forEach((column, columnIndex) => {
            data_line[column] = line[columnIndex];
          });
        } else {
          // store data as an array
          data_line = line;
        }
        if (line_processor_callback) {
          //  call line processor callback if it exists
          data_line = line_processor_callback(data_line, line_index);
        }
        data.push(data_line);
      }
      line_index++;
    });
    line_reader.on('close', () => {
      if(line_filter_callback){
        //  call line filter if it exists
        data = data.filter( (city) => line_filter_callback(city));
      }
      resolve(data);
    });
  });
}

module.exports = dataImporter;
