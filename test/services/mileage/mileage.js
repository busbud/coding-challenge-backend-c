require('../test_helper');

describe('MileageService', function() {

  var emptyFn = function emptyFn() {};


  // stub-ed API
  var _utilMethodsAPI = {
    getLowerCaseUnidecodedString: emptyFn,
    haversineDistance: emptyFn
  };

  var mileageService = null;
  var utilMethodsApiStub = null;


  var testData = readFileAsJSON(__dirname + '/mileage_data.json');


  describe('#computeDistance()', function() {

    beforeEach(function() {

      utilMethodsApiStub = _.extend({}, _utilMethodsAPI);
      mileageService = proxyquire('../../services/mileage-service', {
        '../utils/util_methods': utilMethodsApiStub
      });

    });


    it('should succeed', function(done) {
      // prepare test data
      expect(testData).to.have.property('correctValuesSet');
      var successOpts = testData.correctValuesSet;

      // We just want to test our function
      // so we stub the haversineDistance function
      // and we could test this one in another test case suite
      sinon
        .stub(utilMethodsApiStub, 'haversineDistance',
          function(opts) {
            return 100.0;
          });

      var promise = mileageService.computeDistance(successOpts);
      promise
        .then(function() {
          // If we are there it means that promise has succeed
          // we just have to look at the call count of the dependency
          expect(utilMethodsApiStub.haversineDistance).to.have.callCount(1);
          done();
        }, null)
        .done(); //done chain

    });


    it('should failed because of bad params', function(done) {

      expect(testData).to.have.property('optsNoDeparture');
      expect(testData).to.have.property('optsNoArrival');
      expect(testData).to.have.property('optsMissingLatitudeInDeparture');

      var optsNoDeparture = testData.optsNoDeparture;
      var optsNoArrival = testData.optsNoArrival;
      var optsMissingLatitudeInDeparture = testData.optsMissingLatitudeInDeparture;


      var promiseNoParam = mileageService.computeDistance();
      var promiseParamNotAnObject = mileageService.computeDistance(24);

      var promiseNoDepartureParam = mileageService.computeDistance(optsNoDeparture);
      var promiseNoArrivalParam = mileageService.computeDistance(optsNoArrival);
      var promiseMissingLatitudeInDeparture = mileageService.computeDistance(optsMissingLatitudeInDeparture);


      var group = Q.all([
        expect(promiseNoParam).to.be.rejected,
        expect(promiseParamNotAnObject).to.be.rejected,

        expect(promiseNoDepartureParam).to.be.rejected,
        expect(promiseNoArrivalParam).to.be.rejected,
        expect(promiseMissingLatitudeInDeparture).to.be.rejected
      ]);

      expect(group).to.be.fulfilled.and.notify(done);

    });



  });

});
