let expect  = require('chai').expect

const {
  searchString,
  scoreCity,
  distanceBetweenCoordinates,
  normalizedDistanceBetweenCoordinates,
  cleanAndNormalizeString,
} = require('../../domain/suggestor.helper');

describe('suggestor.helper', function(){
  describe('searchString', function(){
    describe('with invalid inputs', function(){
      it('should return no match', function(){
        let search = searchString(null,'Montréal');
        expect(search).to.include({
          found: false,
        })
      });
      it('should return no match', function(){
        let search = searchString('Montréal',null);
        expect(search).to.include({
          found: false,
        })
      });
      it('should return no match', function(){
        let search = searchString(null,null);
        expect(search).to.include({
          found: false,
        })
      });
    });
    describe('with accents in search pattern', function(){
      it('should find match é in pattern with confidence 1', function(){
        let search = searchString('Montréal','Montréal');
        expect(search).to.include({
          found: true,
          confidence: 1
        })
      });
      it('should find match e in pattern with confidence 1', function(){
        let search = searchString('Montréal','Montreal');
        expect(search).to.include({
          found: true,
          confidence: 1
        })
      });
      it('should find match ê in pattern with confidence 1', function(){
        let search = searchString('Montréal','Montrêal');
        expect(search).to.include({
          found: true,
          confidence: 1
        })
      });
      it('should find match ë in pattern with confidence 1', function(){
        let search = searchString('Montréal','Montrëal');
        expect(search).to.include({
          found: true,
          confidence: 1
        })
      });
    });
    describe('with wrong case', function(){
      it('should find match pattern with confidence 1', function(){
        let search = searchString('Montréal','MOnTRÉaL');
        expect(search).to.include({
          found: true,
          confidence: 1
        })
      });
    });
    describe('with partial match', function(){
      it('should match with correct confidence score', function(){
        let search = searchString('Montréal','Mont');
        expect(search).to.include({
          found: true,
          confidence: (4.0/8.0)
        })
      });
      it('should match with correct confidence score', function(){
        let search = searchString('Montréal','M');
        expect(search).to.include({
          found: true,
          confidence: (1.0/8.0)
        })
      });
      it('should match with correct confidence score', function(){
        let search = searchString('Montréal','Montrea');
        expect(search).to.include({
          found: true,
          confidence: (7/8.0)
        })
      });
    });
    describe('with no match',function(){
      it('should not match pattern', function(){
        let search = searchString('Montréal','zzzzz');
        expect(search).to.include({
          found: false,
        })
      });
    });
  });
  describe('distanceBetweenCoordinates', function(){
    describe('with valid inputs', function(){
      it('should return correct distance', function(){
        let distance = distanceBetweenCoordinates({latitude: 45.50884, longitude: -73.58781},{latitude: 45.50884, longitude: -73.58781});
        expect(distance).to.eql(0);
      });
      it('should return correct distance', function(){
        let distance = distanceBetweenCoordinates({latitude: 0, longitude: -90},{latitude: 0, longitude: 90});
        expect(distance).to.be.within(20015,20016);
      });
      it('should return correct distance', function(){
        let distance = distanceBetweenCoordinates({latitude: -90, longitude: 0},{latitude: 90, longitude: 0});
        expect(distance).to.be.within(20015,20016);
      });
    });
  });
  describe('normalizedDistanceBetweenCoordinates', function(){
    describe('with valid inputs', function(){
      it('should return correct distance', function(){
        let distance = normalizedDistanceBetweenCoordinates({latitude: 45.50884, longitude: -73.58781},{latitude: 45.50884, longitude: -73.58781});
        expect(distance).to.eql(1);
      });
      it('should return correct distance', function(){
        let distance = normalizedDistanceBetweenCoordinates({latitude: 0, longitude: -90},{latitude: 0, longitude: 90});
        expect(distance).to.be.within(0.0,0.0012);
      });
      it('should return correct distance', function(){
        let distance = normalizedDistanceBetweenCoordinates({latitude: -90, longitude: 0},{latitude: 90, longitude: 0});
        expect(distance).to.be.within(0.0,0.0012);
      });
    });
  });
  describe('cleanAndNormalizeString', function(){
    describe('with accents', function(){
      it('should remove è', function(){
        expect(cleanAndNormalizeString('Montrèal')).to.eql('montreal');
      });
      it('should remove é', function(){
        expect(cleanAndNormalizeString('Montréal')).to.eql('montreal');
      });
      it('should remove ê', function(){
        expect(cleanAndNormalizeString('Montrêal')).to.eql('montreal');
      });
      it('should remove ë', function(){
        expect(cleanAndNormalizeString('Montrëal')).to.eql('montreal');
      });
      it('should remove ē', function(){
        expect(cleanAndNormalizeString('Montrēal')).to.eql('montreal');
      });
      it('should remove ė', function(){
        expect(cleanAndNormalizeString('Montrėal')).to.eql('montreal');
      });
      it('should remove ę', function(){
        expect(cleanAndNormalizeString('Montręal')).to.eql('montreal');
      });
    });
    describe('with upper case', function() {
      it('should bring it down to lower case', function(){
        expect(cleanAndNormalizeString('MoNTréAl')).to.eql('montreal');
      });
    });
  });
  describe('scoreCity', function(){
  });
});
