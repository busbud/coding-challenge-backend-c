/**
 * Test cases for the service suggestions.get
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

var expect      = require('chai').expect,
    querystring = require('querystring')
    is          = require('is_js')
    supertest   = require('supertest');

var app         = null,
    request     = null;

// Wait for the system to load
before(function(done) {

  // Disable the logging
  var log = console.log;
  console.log = new Function();

  // Load the app and wait until it finish
  require('../app').then(function(server) {

    app = server;

    // Enable the logging again
    console.log = log;

    // Attach the server
    request = supertest(server);

    // Move to the next step
    done();
    
  });

});

// When all tests are executed, cleanup
after(function() {

  app.close();
  
});

// Test the suggestions.get service
describe('GET /v1/suggestions', function() {

  /**
   * A list of queries and expected first suggestion
   * @type {Array} [{params, expectedFirstSuggestionName, expectedFirstSuggestionScore}, ...]
   */
  var testCases = [
    {
      params: {q: 'SomeRandomCityInTheMiddleOfNowhere'},
      expectedFirstSuggestionName: undefined,
      expectedFirstSuggestionScore: undefined
    },
    {
      params: {q: 'New York'},
      expectedFirstSuggestionName: 'New York City, NY, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'New123244142York'},
      expectedFirstSuggestionName: 'New York City, NY, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'New12324  4142York'},
      expectedFirstSuggestionName: 'New York City, NY, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'New123--##24  4142York'},
      expectedFirstSuggestionName: 'New York City, NY, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'New'},
      expectedFirstSuggestionName: 'New York City, NY, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'NewY'},
      expectedFirstSuggestionName: 'New York City, NY, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'Neww York'},
      expectedFirstSuggestionName: 'New York City, NY, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'Los Ang'},
      expectedFirstSuggestionName: 'Los Angeles, CA, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'Ottawa'},
      expectedFirstSuggestionName: 'Ottawa, ON, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'otaw'},
      expectedFirstSuggestionName: 'Ottawa, ON, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'Otaw'},
      expectedFirstSuggestionName: 'Ottawa, ON, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'Saint-Augustin-de-Desmaures'},
      expectedFirstSuggestionName: 'Saint-Augustin-de-Desmaures, QC, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'Oakvi'},
      expectedFirstSuggestionName: 'Oakville, ON, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'Blainville'},
      expectedFirstSuggestionName: 'Blainville, QC, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'Atlanta'},
      expectedFirstSuggestionName: 'Atlanta, GA, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'atlanta'},
      expectedFirstSuggestionName: 'Atlanta, GA, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'alanta'},
      expectedFirstSuggestionName: 'Atlanta, GA, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'San Jose'},
      expectedFirstSuggestionName: 'San Jose, CA, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'SanJose'},
      expectedFirstSuggestionName: 'San Jose, CA, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'Montreal'},
      expectedFirstSuggestionName: 'Montreal, QC, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'montreal'},
      expectedFirstSuggestionName: 'Montreal, QC, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'monetreal'},
      expectedFirstSuggestionName: 'Montreal, QC, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'montereal'},
      expectedFirstSuggestionName: 'Montreal, QC, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'mont'},
      expectedFirstSuggestionName: 'Montreal, QC, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'mo'},
      expectedFirstSuggestionName: 'Montreal, QC, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'London'},
      expectedFirstSuggestionName: 'London, ON, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'Lendon'},
      expectedFirstSuggestionName: 'London, ON, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'Londonderry'},
      expectedFirstSuggestionName: 'Londonderry, NH, US',
      expectedFirstSuggestionScore: 1
    },
    {
      params: {q: 'lond'},
      expectedFirstSuggestionName: 'London, ON, CA',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'lo'},
      expectedFirstSuggestionName: 'Los Angeles, CA, US',
      expectedFirstSuggestionScore: null
    },
    {
      params: {q: 'Londo', latitude: 43.70011, longitude: -79.4163},
      expectedFirstSuggestionName: 'London, ON, CA',
      expectedFirstSuggestionScore: null
    }
  ];

  describe('with a non-existent city', function() {

    var response;

    before(function(done) {
      request
        .get('/v1/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 404', function() {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', function() {
      expect(response.json.data.suggestions).to.be.instanceof(Array);
      expect(response.json.data.suggestions).to.have.length(0);
    });

  });

  describe('with a valid city', function() {

    var response;

    before(function(done) {
      request
        .get('/v1/suggestions?q=Montreal')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', function() {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', function() {
      expect(response.json.data.suggestions).to.be.instanceof(Array);
      expect(response.json.data.suggestions).to.have.length.above(0);
    });

    it('contains a match', function() {
      expect(response.json.data.suggestions).to.satisfy(function(suggestions) {
        return suggestions.some(function(suggestion) {
          return /montreal/i.test(suggestion.name);
        });
      })
    });

    it('contains latitudes and longitudes', function() {
      expect(response.json.data.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      })
    });

    it('contains scores', function() {
      expect(response.json.data.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      })
    });

  });

  // Foreach defiend text case
  testCases.forEach(function(testCase) {

    // Generate query string
    var params = querystring.stringify(testCase.params);

    describe(`search by the query '${params}'`, function() {

      var response;

      before(function(done) {
        request
          .get('/v1/suggestions?' + params)
          .end(function(err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });

      // Check for no suggestions
      if (is.undefined(testCase.expectedFirstSuggestionName)) {

        it(`should returns no suggestions`, function() {
          expect(response.json.data.suggestions.length).to.equal(0);
        });

      }

      // Check for name
      if (testCase.expectedFirstSuggestionName) {

        it('should returns 1 or more suggestions', function() {
          expect(response.json.data.suggestions).to.have.length.above(0);
        });

        it(`the first suggestion's name should be ${testCase.expectedFirstSuggestionName}`, function() {
          expect(response.json.data.suggestions[0].name).to.equal(testCase.expectedFirstSuggestionName);
        });

      }

      // Check for score
      if (testCase.expectedFirstSuggestionScore) {

        it(`the first suggestion's score should be ${testCase.expectedFirstSuggestionScore}`, function() {
          expect(response.json.data.suggestions).to.have.length.above(0);
          expect(response.json.data.suggestions[0].score).to.equal(testCase.expectedFirstSuggestionScore);
        });

      }

    });
    
  });

});
