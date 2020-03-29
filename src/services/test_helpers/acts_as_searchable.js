import _ from 'lodash';
import chai, { expect } from 'chai';
import chaiSorted from 'chai-sorted';

chai.use(chaiSorted);

const requiredMethods = ['search'];

// async function readTestExample (testExampleFilePath) {
//   const filehandle = await fs.promises.open(testExampleFilePath, 'r');
//   const testExampleContent = await filehandle.readFile();
//   return JSON.parse((testExampleContent));
// }

const itShouldRequiredMethods = (searchService) => {
  requiredMethods.forEach((method) => {
    it(`should implement ${method}`, () => {
      expect(typeof (searchService[method])).to.eql('function');
    });
  });
};

const searchFunctionBehavior = async (searchService, examples) => {
  it('should return a promise', () => {
    expect(searchService.search({ q: 'aText' })).to.be.an.instanceof(Promise);
  });

  _.each(examples, (example) => {
    const { desc, searchQuery, expectedFn } = example;
    it(desc, (done) => {
      expect(expectedFn).to.be.an.instanceof(Function);
      searchService.search(searchQuery)
        .then(actualData => {
          expectedFn(actualData);
          done();
        })
        .catch(err => done(err))
      ;
    });
  });
  // it('should return score in ordered', (done) => {
  //   searchService.search({ q: 'Lon' })
  //     .then(cities => {
  //       expect(cities).to.be.sortedBy('score', { descending: true });
  //       done();
  //     })
  //     .catch((err) => done(err));
  // });
  //
  // it('should return nearest location first', (done) => {
  //   searchService.search({
  //     q: 'Lon',
  //     latitude: 45.53151,
  //     longitude: -73.51806,
  //   })
  //     .then(cities => {
  //       expect(cities).to.be.sortedBy('score', { descending: true });
  //       expect(cities[0].name).to.be.equal('Longueuil');
  //       done();
  //     })
  //     .catch((err) => done(err));
  // });
};

export function actAsSearchService ({ searchService, examples }) {
  return function () {
    describe(`${searchService.constructor.name} Searchable`, () => {
      context('implemented methods', () => itShouldRequiredMethods(searchService));
      context('search behavior', () => searchFunctionBehavior(searchService, examples));
    });
  };
}
