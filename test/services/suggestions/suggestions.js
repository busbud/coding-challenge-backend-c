require('../test_helper');

describe('SuggestionsService', function() {

  var emptyFn = function emptyFn() {};


  // stub-ed API
  var _suggestionsRepositoryAPI = {
    getLowerCaseUnidecodedString: emptyFn,
    haversineDistance: emptyFn
  };

  var suggestionsService = null;
  var suggestionsRepositoryApiStub = null;


  var testData = readFileAsJSON(__dirname + '/suggestions_data.json');


  describe('#suggestCityForOptions()', function() {

    beforeEach(function() {

      suggestionsRepositoryApiStub = _.extend({}, _suggestionsRepositoryAPI);
      suggestionsService = proxyquire('../../services/suggestions-service', {
        '../repositories/suggestions-repository': suggestionsRepositoryApiStub
      });

    });


    it('should succeed', function(done) {

      expect(testData).to.have.property('successParam');
      var successOpts = testData.successParam;

      sinon
        .stub(suggestionsRepositoryApiStub, 'getCityBeginingWith',
          function(name) {
            return Q.fcall(function() {
              // We don't care about what is returned
              // It would be nice to test this function
              // in another test case suite
              return [];
            });
          });

      var promise = suggestionsService.suggestCityForOptions(successOpts);
      promise
        .then(function() {
          // If we are there it means that promise has succeed
          // we just have to look at the call count of the dependency
          expect(suggestionsRepositoryApiStub.getCityBeginingWith).to.have.callCount(1);
          done();
        }, null)
        .done(); //done chain

    });


    it('should failed because of params', function(done) {

      expect(testData).to.have.property('noNameParam');
      expect(testData).to.have.property('blankNameParam');
      expect(testData).to.have.property('latitudeParamMissing');

      var optsNoName = testData.noNameParam;
      var optsBlankName = testData.blankNameParam;
      var optsLatitudeMissing = testData.latitudeParamMissing;

      var promiseNoParam = suggestionsService.suggestCityForOptions();

      var promiseNoName = suggestionsService.suggestCityForOptions(optsNoName);
      var promiseBlankName = suggestionsService.suggestCityForOptions(optsBlankName);
      var promiseLatitudeMissing = suggestionsService.suggestCityForOptions(optsLatitudeMissing);


      var group = Q.all([
        expect(promiseNoParam).to.be.rejected,

        expect(promiseNoName).to.be.rejected,
        expect(promiseBlankName).to.be.rejected,
        expect(promiseLatitudeMissing).to.be.rejected
      ]);

      expect(group).to.be.fulfilled.and.notify(done);

    });


  });

  // Other functions like DTO computation can be test too
  // ...

});
