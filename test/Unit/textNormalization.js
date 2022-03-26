const { normalize } = require('../../src/textNormalization');
const expect = require('chai').expect;

describe('text normalization function', function () {
    it('transforms to lowercase', function () {
        const upperCase = 'ABC';
        const normalizedText = normalize(upperCase);

        expect(normalizedText).to.equal('abc');
    });

    it('Removes é diacritics', function () {
        const sample = 'montréal';
        const normalizedText = normalize(sample);

        expect(normalizedText).to.equal('montreal');
    });

    it('returns empty string when input is falsy', function () {
        const sample = null;
        const normalizedText = normalize(sample);

        expect(normalizedText).to.equal('');
    });
});
