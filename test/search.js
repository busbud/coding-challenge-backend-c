var expect  = require('chai').expect;
var geoCode = require('../src/utils/geoCode');

describe('Get Canadian code', function() {
  describe('with a bad code', function () {
    var code = "432";

    var response = geoCode.getCanadianCode(code);

    it('returns the same code', function () {
      expect(response).to.equal(code);
    });

  });

  describe('with good codes', function () {
    var codes = ["01","02","03","04","05","07","08","09","10","11","12","13","14"];
    var response = [];

    codes.forEach(code => {
        response.push(geoCode.getCanadianCode(code));
    });

    it('returns list of Canadia codes', function () {
        expect(response).to.eql(["AB","BC","MB","NB","NL","NS","ON","PE","QC","SK","YT","NT","NU"]);
    });
  });

});