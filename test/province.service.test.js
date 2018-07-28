const chai = require('chai');
const expect = chai.expect;
chai.use(require('chai-as-promised'));
chai.use(require('sinon-chai'));

const sinon = require('sinon');
const ProvinceService = require('../lib/province.service');

describe('ProvinceService', () => {
  let mongoClient;
  let provinceService;

  beforeEach(() => {
    findOne = sinon.stub();

    mongoClient = {
      db: () => ({
        collection: () => ({
          findOne
        })
      })
    };

    provinceService = new ProvinceService(mongoClient);
  });

  describe('getProvinceByCode function', () => {
    it('should invoke mongo with a findOne query if province is not found in cache, and put the result in cache', () => {
      findOne.resolves({
        name: 'mock'
      });

      return expect(provinceService.getProvinceByCode('unit')).to.be.fulfilled.then(r => {
        expect(findOne).to.have.been.calledWith({ code: 'unit' });

        expect(provinceService.cache.length).to.equal(1);

        return expect(r).to.deep.equal({
          name: 'mock'
        });
      });
    });

    it('should not invoke mongo with a findOne query if province is found in cache', () => {
      findOne.resolves({
        name: 'mockMongo'
      });

      provinceService.cache.set('unit', {
        name: 'mockCache'
      });

      return expect(provinceService.getProvinceByCode('unit')).to.be.fulfilled.then(r => {
        expect(findOne).to.not.have.been.called;

        return expect(r).to.deep.equal({
          name: 'mockCache'
        });
      });
    });
  });
});
