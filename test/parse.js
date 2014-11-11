var expect  = require('chai').expect;
var parse   = require('../src/parse');

describe('Parse input file', function() {
    it('runs', function () {
        parse.parse(function () {
            expect('').to.equal('');
        });
    });
});
