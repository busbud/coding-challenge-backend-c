var expect  = require('chai').expect;
var parse   = require('../src/parse');

describe('Parse input file', function() {
    it('Geoname Gazetteer Extract TSV file', function () {
        parse.parse(function (output) {
            expect(output.length).to.be.greaterThan(0);
            expect(output[0].name).to.be("Abbotsford");
        });
    });
});
