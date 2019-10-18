const expect = require('chai').expect;
const sinon = require('sinon');
const fs = require('fs');
const { Readable } = require('stream');
const { readCityData } = require('../readCityData');

describe('readCityData', () => {
  function mockFileStream(linesToRead) {
    const fakeStream = new Readable();
    for (const lineToRead of linesToRead) {
      fakeStream.push(lineToRead);
    }
    fakeStream.push(null);
    sinon.replace(fs, 'createReadStream', sinon.fake.returns(fakeStream));
  }

  describe('two valid cities', () => {
    beforeEach(() => {
      mockFileStream(['id\tname\n', '1\twinnipeg\n', '2\tmontreal\n']);
    });

    it('returns two objects representing the cities', async () => {
      const cityData = await readCityData('');
      expect(cityData).length(2);
    });

    it('should parse cities according to the headers of the file', async () => {
      const cityData = await readCityData('');
      expect(cityData[0].name).equals('winnipeg');
      expect(cityData[0].id).equals('1');
      expect(cityData[1].name).equals('montreal');
      expect(cityData[1].id).equals('2');
    });
  });

  describe('one valid city, one invalid city', () => {
    before(() => {
      mockFileStream(['id\tname\n', '1\n', '2\tmontreal\n']);
    });

    it('ignores the invalid city', async () => {
      const cityData = await readCityData('');
      expect(cityData).length(1);
      expect(cityData[0].name).equal('montreal');
    });
  });

  afterEach(() => {
    sinon.restore();
  });
});
