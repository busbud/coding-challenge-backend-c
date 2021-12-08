var assert = require('chai').assert;
var utils = require('../src/utils');

describe('Utils test', function () {

    describe('getCountry', function () {
        it('Canada', async () => {
            assert.equal('Canada', utils.getCountry("CA"))
        });
        it('USA', async () => {
            assert.equal('USA', utils.getCountry("US"))
        });
        it('DUMMY', async () => {
            assert.equal('N/A', utils.getCountry("NA"))
        });
        it('null/undefined', async () => {
            assert.equal('N/A', utils.getCountry(null))
            assert.equal('N/A', utils.getCountry(undefined))
        });
    });
    describe('calculate dist', function () {
        it('Res = 0', async () => {
            assert.equal(0, utils.distanceLonLat(43.001, 53.001, 43.001, 53.001))
        });
        it('Res != 0', async () => {
            assert.equal(11119.49266445587, utils.distanceLonLat(43.001, 53.001, 53.001, 53.001))
        });
    });
});
