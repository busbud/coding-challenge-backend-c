
/********************
 * Import Modules
 *******************/

var _ = require("lodash");

var Classy = require("../structures/classy.js");




/********************
 * Data
 *******************/

// Basic object to store the data from a database (whatever with columns, CSV, TSV)
// Can access to the data by array reference for looping and by hash for speed
var Data = Classy.extend({
  constructor: function (elementsList, idName) {
    // Set up id logic; it can be given or by default will take id; it should be the same name as on the DB column
    this._idName = this._idName || idName || "id";
    this._columnsList = _.clone(elementsList);
    this._formatHash = {};
    for (var i = 0; i < this._columnsList.length; i++) {
      this._formatHash[this._columnsList[i]] = null;
    }

    // Prepare for input
    this._dataList = [];
    this._dataHash= {};
  },

  // Row validator to be overriden
  // If changes in the data is necessary, just overwrite
  validate: function (row) {
    return row;
  },

  // Getters for meta information
  getColumnList: function () {
    return this._columnsList;
  },
  getFormatHash: function () {
    return this._formatHash;
  },

  // Getters for data
  getElementByID: function (id) {
    if (_.isString(id) || _.isNumber(id)) {
      return this._dataHash[id];
    } else {
      return null;
    }
  },
  getElementByIndex: function (index) {
    if (_.isNumber(index) && (index >= 0) && (index < this._dataList.length)) {
      return this._dataList[index];
    } else {
      return null;
    }
  },
  getDataList: function () {
    return this._dataList;
  },
  getDataHash: function () {
    return this._dataHash;
  },

  // Interface for adding elements
  addRow: function (rowElements) {
    if (_.isArray(rowElements)) {
      if (rowElements.length === this._columnsList.length) {
        // Structure data like "this._formatHash"
        var _row = {};
        for (var i = 0; i < rowElements.length; i++) {
          _row[this._columnsList[i]] = rowElements[i];
        }

        // Check if ID does not exist in row
        if (!_row[this._idName]) {
          // Check if empty String
          if (_row[this._idName] === "") {
            // !!! This method is used during initialization and not during runtime, exceptions allowed !!!
            throw new Error("Busbud C - Data: ID name in row is bad. Empty string.");
          } else {
            // Generate ID since row does not have one
            _row[this._idName] = _.uniqueId();
          }
        }

        // Check if ID is already used in hash
        if (this._dataHash[_row[this._idName]]) {
          // !!! This method is used during initialization and not during runtime, exceptions allowed !!!
          throw new Error("Busbud C - Data: Duplication of supposedly unique ID. ID: " + _row[this._idName] + ".");
        }

        // Check row validation
        var _validatedRow = this.validate(_row);
        if (_validatedRow) {
          // Check that ID remains untouched
          if (!(_validatedRow[this._idName]) || (_validatedRow[this._idName] !== _row[this._idName])) {
            // This should not happen lol
            // !!! This method is used during initialization and not during runtime, exceptions allowed !!!
            throw new Error("Busbud C - Data: Validation function is bad. Changed ID from data.");
          }

          this._dataList.push(_validatedRow);
          this._dataHash[_validatedRow[this._idName]] = _validatedRow;
        }
      } else {
        // !!! This method is used during initialization and not during runtime, exceptions allowed !!!
        throw new Error("Busbud C - Data: Row to be added does not satisfy the size of the format described. " +
          "Columns number: " + this._columnsList.length + ". Row columns number: " + rowElements.length +
          ". Bad escaping in Data class' logic or source is probably corrupted.");
      }
    } else {
      // !!! This method is used during initialization and not during runtime, exceptions allowed !!!
      throw new Error("Busbud C - Data: Row to be added to data instance is not an array.");
    }
  }
});





/********************
 * Validators
 *******************/

var CanadaAdmin1Codes = {
  "00": null, // General canada
  "01": "AL",
  "02": "BC",
  "03": "MB",
  "04": "NB",
  "05": "NL",
  "07": "NS",
  "08": "ON",
  "09": "PE",
  "10": "QC", // I know this place :D
  "11": "SK",
  "12": "YT",
  "13": "NT",
  "14": "NU"
};

// Validator for Busbud Geonames standard
// Not very effective but it is just to format better the JSON for city information
function BusbudGeonamesValidator(row) {
  // Check feat_class, only accept cities here
  if (row.feat_class !== "P") {
    return null;
  }

  // New object to be returned, filtered
  _data = {};
  _data.id = row.id;
  _data.name = row.name;
  _data.name_alt = row.alt_name;
  _data.population = parseInt(row.population);
  _data.geo = {};
  _data.geo.lat = parseInt(row.lat);
  _data.geo.long = parseInt(row.long);
  _data.geo.elevation = parseInt(row.elevation);
  _data.geo.dem = parseInt(row.dem);
  _data.country = row.country;
  _data.tz = row.tz;

  // Admin 1 codes
  if (row.country === "CA") {
    _data.province = CanadaAdmin1Codes[row.admin1];
  } else if (row.country === "US") {
    _data.province = row.admin1;
  }

  return _data;
}




/********************
 * Export Modules
 *******************/

module.exports = Data;

module.exports.BusbudGeonamesValidator = BusbudGeonamesValidator;
