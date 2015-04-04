
var assert = require('assert');
global.assert = assert;

var chai = require('chai');
var sinonChai = require('sinon-chai');
var chaiAsPromised = require('chai-as-promised');

chai.use(sinonChai);
chai.use(chaiAsPromised);

global.expect = chai.expect;
global.sinon = require('sinon');

var proxyquire = require('proxyquire');
global.proxyquire = proxyquire;

var Q = require('q');
global.Q = Q;


// var should = require('should');
var should = require('chai').should();
global.should = should;


// helper read file
var _ = require('../../utils/underscore_with_string');
global._ = _;



var fsModule = require('fs');
var readFileAsJSON = function readFileAsJSON(filepath) {
  if (_.isString(filepath) === false) {
    throw new Error('readFileAsJSON: filepath is not a string');
  }
  if (_.str.isBlank(filepath)) {
    throw new Error('readFileAsJSON: filepath is blank');
  }

  if (fsModule.existsSync(filepath) === false) {
    throw new Error('readFileAsJSON: filepath doesn\'t exists: ' + filepath);
  }

  var content = fsModule.readFileSync(filepath);

  if (_.isEmpty(content)) {
    throw new Error('Empty content of: ' + filepath);
  }

  var contentJSON;
  try {
    contentJSON = JSON.parse(content);
  } catch (parseErr) {
    console.log('Failed to parse content of %s', filepath);
    throw parseErr;
  }

  return contentJSON;
};

global.readFileAsJSON = readFileAsJSON;
