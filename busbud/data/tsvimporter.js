
/********************
 * Import Modules
 *******************/

var _ = require("lodash");

var FS = require("fs");
var ES = require("event-stream");

var Classy = require("../structures/classy.js");

var Data = require("./data.js");




/********************
 * TSV Importer
 *******************/

var TSVImporter = Classy.extend({
  // @param: Callback is a function(err, data) {...} where data is a Data class object
  constructor: function (filename, dataClass, callback) {
    // Parameter Setup
    if (arguments.length === 2) {
      if (dataClass instanceof Function) {
        callback = dataClass;
        dataClass = null;

        if ((this.dataClass === null) || (this.dataClass === undefined)) {
          throw new Error("Busbud C - TSVImporter: No Data class has been passed.");
        }
      }
    }

    // Setup
    this.data = null;
    this.lineNumber = 0;
    this.dataClass = this.dataClass || dataClass;

    this.pipeProcessLine = _.bind(this._pipeProcessLine, this);
    this.streamOnFinish = _.bind(this._streamOnFinish, this, callback);
    this.streamOnError = _.bind(this._streamOnError, this, callback);

    // Follow the busbud guidance on the ninja event-streaming
    try {
      // Create stream
      var _tsvStream = FS.createReadStream(filename, { flags: "r", encoding: "utf8" });

      // Events
      _tsvStream.on("end", this.streamOnFinish);
      _tsvStream.on("error", this.streamOnError);

      // Start pumping
      _tsvStream.pipe(ES.split())
        .pipe(ES.map(this.pipeProcessLine));
        //.pipe(FS.createWriteStream("/dev/null")); // data is useless?
    } catch (err) {
      // An error happened during the file I/O
      return callback(err)
    }
  },

  // Function to be overloaded if necessary to do something with the data
  processData: function (data) {},


  // Process each line
  _pipeProcessLine: function (row, callback) {
    // Check that row is not empty
    if (row === "") {
      return callback(null);
    }

    // Spilt all columns into separate strings
    var _rowElements = row.split("\t"); // No need to check that data is string since file is opened in UTF8 mode

    // Check The line we are currently at
    if (this.lineNumber === 0) { // First line of TSV is meta info
      // Create the data object
      if (this.dataClass) {
        this.data = new this.dataClass(_rowElements);
      } else {
        this.data = new Data(_rowElements);
      }
    } else {
      // Add elements to data
      if (this.data) {
        this.data.addRow(_rowElements);
      } else {
        return callback("Busbud C - TSVImporter: Data object does not exist on row importing.");
      }
    }

    // Increase line counter and return
    this.lineNumber++;
    return callback(null);
  },

  // Finish end stream event
  _streamOnFinish: function (callback) {
    this.processData(this.data);
    return callback(null, this.data);
  },

  // Error event
  _streamOnError: function (callback, err) { // For once that callback is first argument and error second :P
    return callback(err);
  }
});




/********************
 * Export Modules
 *******************/

module.exports = TSVImporter;