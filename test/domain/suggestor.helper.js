const expect = require('chai').expect;
const suggestionConfig = require('../../config').suggestionConfig;

const {
  searchString,
  scoreCity,
  distanceBetweenCoordinates,
  normalizedDistanceBetweenCoordinates,
  cleanAndNormalizeString
} = require('../../domain/suggestor.helper');

describe('suggestor.helper', function() {
  describe('searchString', function() {
    describe('with invalid inputs', function() {
      it('should return no match', function() {
        const search = searchString(null, 'Montréal');
        expect(search).to.include({
          found: false
        });
      });
      it('should return no match', function() {
        const search = searchString('Montréal', null);
        expect(search).to.include({
          found: false
        });
      });
      it('should return no match', function() {
        const search = searchString(null, null);
        expect(search).to.include({
          found: false
        });
      });
    });
    describe('with accents in search pattern', function() {
      it('should find match é in pattern with confidence 1', function() {
        const search = searchString('Montréal', 'Montréal');
        expect(search).to.include({
          found: true,
          confidence: 1
        });
      });
      it('should find match e in pattern with confidence 1', function() {
        const search = searchString('Montréal', 'Montreal');
        expect(search).to.include({
          found: true,
          confidence: 1
        });
      });
      it('should find match ê in pattern with confidence 1', function() {
        const search = searchString('Montréal', 'Montrêal');
        expect(search).to.include({
          found: true,
          confidence: 1
        });
      });
      it('should find match ë in pattern with confidence 1', function() {
        const search = searchString('Montréal', 'Montrëal');
        expect(search).to.include({
          found: true,
          confidence: 1
        });
      });
    });
    describe('with wrong case', function() {
      it('should find match pattern with confidence 1', function() {
        const search = searchString('Montréal', 'MOnTRÉaL');
        expect(search).to.include({
          found: true,
          confidence: 1
        });
      });
    });
    describe('with partial match', function() {
      it('should match with correct confidence score', function() {
        const search = searchString('Montréal', 'Mont');
        expect(search).to.include({
          found: true,
          confidence: (4.0 / 8.0)
        });
      });
      it('should match with correct confidence score', function() {
        const search = searchString('Montréal', 'M');
        expect(search).to.include({
          found: true,
          confidence: (1.0 / 8.0)
        });
      });
      it('should match with correct confidence score', function() {
        const search = searchString('Montréal', 'Montrea');
        expect(search).to.include({
          found: true,
          confidence: (7 / 8.0)
        });
      });
    });
    describe('with no match', function() {
      it('should not match pattern', function() {
        const search = searchString('Montréal', 'zzzzz');
        expect(search).to.include({
          found: false
        });
      });
    });
  });
  describe('distanceBetweenCoordinates', function() {
    describe('with valid inputs', function() {
      it('should return correct distance', function() {
        const distance = distanceBetweenCoordinates({ latitude: 45.50884, longitude: -73.58781 }, { latitude: 45.50884, longitude: -73.58781 });
        expect(distance).to.eql(0);
      });
      it('should return correct distance', function() {
        const distance = distanceBetweenCoordinates({ latitude: 0, longitude: -90 }, { latitude: 0, longitude: 90 });
        expect(distance).to.be.within(20015, 20016);
      });
      it('should return correct distance', function() {
        const distance = distanceBetweenCoordinates({ latitude: -90, longitude: 0 }, { latitude: 90, longitude: 0 });
        expect(distance).to.be.within(20015, 20016);
      });
    });
  });
  describe('normalizedDistanceBetweenCoordinates', function() {
    describe('with valid inputs', function() {
      it('should return correct distance', function() {
        const distance = normalizedDistanceBetweenCoordinates({ latitude: 45.50884, longitude: -73.58781 }, { latitude: 45.50884, longitude: -73.58781 });
        expect(distance).to.eql(1);
      });
      it('should return correct distance', function() {
        const distance = normalizedDistanceBetweenCoordinates({ latitude: 0, longitude: -90 }, { latitude: 0, longitude: 90 });
        expect(distance).to.be.within(0.0, 0.0012);
      });
      it('should return correct distance', function() {
        const distance = normalizedDistanceBetweenCoordinates({ latitude: -90, longitude: 0 }, { latitude: 90, longitude: 0 });
        expect(distance).to.be.within(0.0, 0.0012);
      });
    });
  });
  describe('cleanAndNormalizeString', function() {
    describe('with accents', function() {
      it('should remove è', function() {
        expect(cleanAndNormalizeString('Montrèal')).to.eql('montreal');
      });
      it('should remove é', function() {
        expect(cleanAndNormalizeString('Montréal')).to.eql('montreal');
      });
      it('should remove ê', function() {
        expect(cleanAndNormalizeString('Montrêal')).to.eql('montreal');
      });
      it('should remove ë', function() {
        expect(cleanAndNormalizeString('Montrëal')).to.eql('montreal');
      });
      it('should remove ē', function() {
        expect(cleanAndNormalizeString('Montrēal')).to.eql('montreal');
      });
      it('should remove ė', function() {
        expect(cleanAndNormalizeString('Montrėal')).to.eql('montreal');
      });
      it('should remove ę', function() {
        expect(cleanAndNormalizeString('Montręal')).to.eql('montreal');
      });
    });
    describe('with upper case', function() {
      it('should bring it down to lower case', function() {
        expect(cleanAndNormalizeString('MoNTréAl')).to.eql('montreal');
      });
    });
  });
  describe('scoreCity', function() {
    const city = {
      ascii: 'Montréal',
      coordinate: {
        latitude: 45.50884,
        longitude: -73.58781
      }
    };

    describe('without search coordinate', function() {
      describe('with a full match', function() {
        it('should return a number', function() {
          expect(scoreCity(city, 'montreal', null)).be.a('number');
        });

        it('should return the correct score', function() {
          expect(scoreCity(city, 'montreal', null)).be.eql(1.0);
        });
      });

      describe('with a partial match', function() {
        it('should return a number', function() {
          expect(scoreCity(city, 'mon', null)).be.a('number');
        });

        it('should return the correct score', function() {
          expect(scoreCity(city, 'mon', null)).be.eql(3.0 / 8.0);
        });
      });

      describe('with no match', function() {
        it('should return a number', function() {
          expect(scoreCity(city, 'foo', null)).be.a('number');
        });

        it('should return the correct score', function() {
          expect(scoreCity(city, 'foo', null)).be.eql(0.0);
        });
      });
    });

    describe('with search coordinate', function() {
      const search_coordinate = { latitude: 45.50884, longitude: -68.7263 };
      const max_distance = 20037.5;
      const distance_apart = 378.8;
      const distance_score_weight = suggestionConfig.coordinateScoreWeight;
      const name_score_weight = (1.0 - suggestionConfig.coordinateScoreWeight);
      const error_tolerance = 1.0 / Math.pow(10, suggestionConfig.scorePrecision);

      describe('with a full match', function() {
        it('should return a number', function() {
          expect(scoreCity(city, 'montreal', search_coordinate)).be.a('number');
        });

        it('should return the correct score', function() {
          const distance_score = (max_distance - distance_apart) / max_distance;
          const expected_score = (1 * name_score_weight) + (distance_score * distance_score_weight);
          const calculate_score = scoreCity(city, 'montreal', search_coordinate);
          expect(calculate_score).be.closeTo(expected_score, error_tolerance);
        });
      });


      describe('with a partial match', function() {
        it('should return a number', function() {
          expect(scoreCity(city, 'mon', search_coordinate)).be.a('number');
        });

        it('should return the correct score', function() {
          const distance_score = (max_distance - distance_apart) / max_distance;
          const expected_score = ((3.0 / 8.0) * name_score_weight) + (distance_score * distance_score_weight);
          const calculate_score = scoreCity(city, 'mon', search_coordinate);
          expect(calculate_score).be.closeTo(expected_score, error_tolerance);
        });
      });

      describe('with no match', function() {
        it('should return a number', function() {
          expect(scoreCity(city, 'foo', search_coordinate)).be.a('number');
        });

        it('should return the correct score', function() {
          const expected_score = 0;
          const calculate_score = scoreCity(city, 'foo', search_coordinate);
          expect(calculate_score).be.eql(expected_score);
        });
      });
    });
  });
});
