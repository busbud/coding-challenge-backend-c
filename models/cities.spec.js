const {expect}  = require('chai');
const {getSuggestions} = require('./cities');

describe('models.cities', () => {
  describe('getSuggestions', () => {
    describe('without required parameters', () => {
      let result;
      before('call function and get result', async () => {
        result = await getSuggestions({foo: 'bar'});
      });

      it('should return an empty array', () => {
        expect(result).to.be.an('array');
        expect(result.length).to.equal(0);
      });
    });

    describe('with only q parameter', () => {
      let result;
      before('call function and get result', async () => {
        result = await getSuggestions({q: 'Mon'});
      });

      it('should return a cities array', () => {
        expect(result).to.be.an('array');
        expect(result.length).to.equal(20);
      });

      it('contains names', () => {
        expect(result).to.satisfy(suggestions => suggestions.every(suggestion => !!suggestion.name));
      });

      it('contains latitudes and longitudes', () => {
        expect(result).to.satisfy(suggestions => suggestions.every(suggestion => suggestion.latitude && suggestion.longitude));
      });

      it('contains scores', () => {
        expect(result).to.satisfy(suggestions => suggestions.every(suggestion => typeof suggestion.score !== 'undefined'));
      });
    });

    describe('with all parameters', () => {
      let result;
      before('call function and get result', async () => {
        result = await getSuggestions({q: 'Mon', longitude: -71.8929, latitude: 45.4042});
      });

      it('should return a cities array', () => {
        expect(result).to.be.an('array');
        expect(result.length).to.equal(20);
      });

      it('contains names', () => {
        expect(result).to.satisfy(suggestions => suggestions.every(suggestion => !!suggestion.name));
      });

      it('contains latitudes and longitudes', () => {
        expect(result).to.satisfy(suggestions => suggestions.every(suggestion => suggestion.latitude && suggestion.longitude));
      });

      it('contains scores', () => {
        expect(result).to.satisfy(suggestions => suggestions.every(suggestion => typeof suggestion.score !== 'undefined'));
      });
    });
  });
});
