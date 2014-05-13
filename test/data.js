
/********************
 * Import Modules
 *******************/

var expect = require("chai").expect;

var Data = require(__dirname + "/../busbud/data/data.js"),
  TSVImporter = require(__dirname + "/../busbud/data/tsvimporter.js");




/********************
 * Test Data
 *******************/

// More tests than code :O bonus points all for me
// All methods are tested of Data
// I was sloppy with naming variables here, please don't take them in consideration too much
describe("Data class", function () {
  // Column formats
  var _columnsWithID = ["id", "c1", "c2"];
  var _columnsWithoutID = ["c1", "c2", "c3", "c4"];

  // Data class
  var _dataWithID = new Data(_columnsWithID, "id");
  var _dataWithoutID = new Data(_columnsWithoutID, "noid");

  // Example rows
  var _badRow = ["the", "most", "bad", "ever", "wanted"];
  var _rowWithID1 = ["1", "a", "b"],
    _rowWithID2 = ["2", "c", "d"],
    _rowWithID3 = ["3", "e", "f"];
  var _rowWithoutID1 = ["a", "b", "c", "d"],
    _rowWithoutID2 = ["e", "f", "g", "h"],
    _rowWithoutID3 = ["i", "j", "k", "l"],
    _rowWithoutID4 = ["m", "n", "o", "p"];

  it("#getColumnList() should be the clone of input", function () {
    expect(_dataWithID.getColumnList().join()).to.be.equal(_columnsWithID.join());
    expect(_dataWithoutID.getColumnList().join()).to.be.equal(_columnsWithoutID.join());
  });
  it("must have a data hash", function () {
    expect(_dataWithID.getDataHash()).to.be.instanceOf(Object);
    expect(_dataWithoutID.getDataHash()).to.be.instanceOf(Object);
  });
  it("#getFormatHas() must have a format with all elements from the columns", function () {
    var _formatWithID = _dataWithID.getFormatHash(),
      _worksWithID = true;
    var _formatWithoutID = _dataWithoutID.getFormatHash(),
      _worksWithoutID = true;

    for (var i = 0; i < _columnsWithID.length; i++) {
      if (!_formatWithID.hasOwnProperty(_columnsWithID[i])) {
        _worksWithID = false;
        break;
      }
    }
    for (var j = 0; j < _columnsWithoutID.length; j++) {
      if (!_formatWithoutID.hasOwnProperty(_columnsWithoutID[j])) {
        _worksWithoutID = false;
        break;
      }
    }
    expect(_worksWithID).to.be.true;
    expect(_worksWithoutID).to.be.true;
  });
  it("#getDataList() should return an empty Array when nothing was added", function () {
    expect(_dataWithID.getDataList()).to.be.instanceOf(Array);
    expect(_dataWithoutID.getDataList()).to.be.instanceOf(Array);
  });
  it("must throw if row added does not correspond with format", function () {
    expect( function () {
      _dataWithID.addRow(_badRow);
    }).to.throw();
    expect( function () {
      _dataWithoutID.addRow(_badRow);
    }).to.throw();
  });
  it("must accept all rows that are ok", function () {
    expect( function () {
      _dataWithID.addRow(_rowWithID1);
      _dataWithID.addRow(_rowWithID2);
      _dataWithID.addRow(_rowWithID3);
    }).to.not.throw();
    expect( function () {
      _dataWithoutID.addRow(_rowWithoutID1);
      _dataWithoutID.addRow(_rowWithoutID2);
      _dataWithoutID.addRow(_rowWithoutID3);
      _dataWithoutID.addRow(_rowWithoutID4);
    }).to.not.throw();
  });
  it("must throw if duplicate ID is being added", function () {
    expect( function () {
      _dataWithID.addRow(_rowWithID1); // it will still be added to array but it throws
    }).to.throw();
  });
  it("must throw if row is not array", function () {
    expect( function () {
      _dataWithID.addRow({});
    }).to.throw();
    expect( function () {
      _dataWithoutID.addRow({});
    }).to.throw(0);
  });
  it("must be able to retrieve a row with index and match clone", function () {
    var _rowDataWithID = _dataWithID.getElementByIndex(1),
      _rowDataWithIDValid = true;
    var _rowDataWithoutID = _dataWithoutID.getElementByIndex(1),
      _rowDataWithoutIDValid = true;

    for (var i = 0; i < _dataWithID.getColumnList().length; i++) {
      if ((_rowDataWithID[_dataWithID.getColumnList()[i]]) != _rowWithID2[i]) {
        _rowDataWithIDValid = false;
        break;
      }
    }
    for (var j = 0; j < _dataWithoutID.getColumnList().length; j++) {
      if ((_rowDataWithoutID[_dataWithoutID.getColumnList()[j]]) != _rowWithoutID2[j]) {
        _rowDataWithoutIDValid = false;
        break;
      }
    }

    expect(_rowDataWithIDValid).to.be.true;
    expect(_rowDataWithoutIDValid).to.be.true;
  });
  it("must be able to load with ID if provided", function () {
    //console.log("Element loaded with ID: ");
    //console.log(_dataWithID.getElementByID(_rowWithID1[0]));
    expect(_dataWithID.getElementByID(_rowWithID1[0])).to.not.be.undefined;
    expect(_dataWithID.getElementByID(_rowWithID1[0])).to.not.be.null;
  });
});




/********************
 * TSV Importer Test
 *******************/

describe("TSVImporter class", function () {
  // Import test data file
  var _tsvTest = null,
    _importingSuccess = true,
    _data = null;

  // Load asynchronously the data, must finish first before tests
  before(function (done) {
    _tsvTest = new TSVImporter(__dirname + "/test-data.tsv", Data, function (err, data) {
      // Check if error
      if (err) {
        _importingSuccess = false;
        return done(err);
      }
      _data = data;
      done(null);
    });
  });

  it("must not return an error for importing valid file", function () {
    expect(_importingSuccess).to.be.true;
  });
  it("must return a valid data class", function () {
    expect(_data).to.be.instanceOf(Data);
  });
  it("must contain data from the file", function () {
    var _rowFileDataHash = _data.getElementByID(1);
    expect(_rowFileDataHash).to.not.be.null;
    expect(_rowFileDataHash).to.not.be.undefined;
    //console.log("Row file data hash: ");
    //console.log(_rowFileDataHash);

    var _rowFileData = [];
    var _columnList = _data.getColumnList();
    //console.log("Column list: " + _columnList.join());
    for (var i = 0; i < _columnList.length; i++) {
      //console.log("Property " + _columnList[i] + " of element: ");
      //console.log(_rowFileDataHash[_columnList[i]]);
      _rowFileData.push(_rowFileDataHash[_columnList[i]]);
    }

    var _rowExpectedData = ["1", "a", "b", "c"];

    expect(_rowFileData.join()).to.equal(_rowExpectedData.join());
  });
  it("must not have more elements than given", function () {
    expect(_data.getDataList().length).to.equal(4);
  });
});
