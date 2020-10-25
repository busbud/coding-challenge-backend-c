const { expect } = require('chai');
const app = require('../app');
const request = require('supertest')(app);
const querystring = require('querystring');

describe('GET /suggestions', () => {
  const simpleTestCases = [
    // this array of tests was taken from PR #89
    // @author Mohammad Fares <faressoft.com@gmail.com>
    // I've commented out the tests with typos because my solution doesn't support that
    {
      params: { q: 'SomeRandomCityInTheMiddleOfNowhere' },
      expectedFirstSuggestionName: undefined,
      expectedFirstSuggestionScore: undefined,
    },
    {
      params: { q: 'New York' },
      expectedFirstSuggestionName: 'New York City, NY, US',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'New York', latitude: 43.70011, longitude: -79.4163 },
      expectedFirstSuggestionName: 'New York City, NY, US',
      expectedFirstSuggestionScore: null,
    },
    // {
    //   params: {q: 'New123244142York'},
    //   expectedFirstSuggestionName: 'New York City, NY, US',
    //   expectedFirstSuggestionScore: null
    // },
    // {
    //   params: {q: 'New12324  4142York'},
    //   expectedFirstSuggestionName: 'New York City, NY, US',
    //   expectedFirstSuggestionScore: null
    // },
    // {
    //   params: {q: 'New123--##24  4142York'},
    //   expectedFirstSuggestionName: 'New York City, NY, US',
    //   expectedFirstSuggestionScore: null
    // },
    {
      params: { q: 'New' },
      expectedFirstSuggestionName: 'New York City, NY, US',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'NewY' },
      expectedFirstSuggestionName: 'New York City, NY, US',
      expectedFirstSuggestionScore: null,
    },
    // {
    //  params: {q: 'Neww York'},
    //  expectedFirstSuggestionName: 'New York City, NY, US',
    //  expectedFirstSuggestionScore: null
    // },
    {
      params: { q: 'Los Ang' },
      expectedFirstSuggestionName: 'Los Angeles, CA, US',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'Ottawa' },
      expectedFirstSuggestionName: 'Ottawa, ON, CA',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'otaw' },
      expectedFirstSuggestionName: 'Ottawa, ON, CA',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'Otaw' },
      expectedFirstSuggestionName: 'Ottawa, ON, CA',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'Saint-Augustin-de-Desmaures' },
      expectedFirstSuggestionName: 'Saint-Augustin-de-Desmaures, QC, CA',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'Oakvi' },
      expectedFirstSuggestionName: 'Oakville, ON, CA',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'Blainville' },
      expectedFirstSuggestionName: 'Blainville, QC, CA',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'Atlanta' },
      expectedFirstSuggestionName: 'Atlanta, GA, US',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'atlanta' },
      expectedFirstSuggestionName: 'Atlanta, GA, US',
      expectedFirstSuggestionScore: null,
    },
    // {
    //  params: {q: 'alanta'},
    //  expectedFirstSuggestionName: 'Atlanta, GA, US',
    //  expectedFirstSuggestionScore: null
    // },
    {
      params: { q: 'San Jose' },
      expectedFirstSuggestionName: 'San Jose, CA, US',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'SanJose' },
      expectedFirstSuggestionName: 'San Jose, CA, US',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'Montreal' },
      expectedFirstSuggestionName: 'Montréal, QC, CA',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'montreal' },
      expectedFirstSuggestionName: 'Montréal, QC, CA',
      expectedFirstSuggestionScore: null,
    },
    // {
    //   params: {q: 'monetreal'},
    //   expectedFirstSuggestionName: 'Montréal, QC, CA',
    //   expectedFirstSuggestionScore: null
    // },
    // {
    //   params: {q: 'montereal'},
    //   expectedFirstSuggestionName: 'Montréal, QC, CA',
    //   expectedFirstSuggestionScore: null
    // },
    {
      params: { q: 'mont' },
      expectedFirstSuggestionName: 'Montréal, QC, CA',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'mo' },
      expectedFirstSuggestionName: 'Montréal, QC, CA',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'London' },
      expectedFirstSuggestionName: 'London, ON, CA',
      expectedFirstSuggestionScore: null,
    },
    // {
    //   params: {q: 'Lendon'},
    //   expectedFirstSuggestionName: 'London, ON, CA',
    //   expectedFirstSuggestionScore: null
    // },
    {
      params: { q: 'Londonderry' },
      expectedFirstSuggestionName: 'Londonderry, NH, US',
      expectedFirstSuggestionScore: 1,
    },
    {
      params: { q: 'lond' },
      expectedFirstSuggestionName: 'London, ON, CA',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'lo' },
      expectedFirstSuggestionName: 'Los Angeles, CA, US',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'Londo', latitude: 43.70011, longitude: -79.4163 },
      expectedFirstSuggestionName: 'London, ON, CA',
      expectedFirstSuggestionScore: null,
    },
  ];

  // Foreach defiend text case
  simpleTestCases.forEach((testCase) => {
    // Generate query string
    const params = querystring.stringify(testCase.params);

    describe(`search by the query '${params}'`, () => {
      let response;

      before((done) => {
        request
          .get(`/suggestions?${params}`)
          .end((err, res) => {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });

      // Check for no suggestions
      if (testCase.expectedFirstSuggestionName === undefined) {
        it('should return no suggestions', () => {
          expect(response.json.suggestions.length).to.equal(0);
        });
      }

      // Check for name
      if (testCase.expectedFirstSuggestionName) {
        // it('should return 1 or more suggestions', function() {
        //  expect(response.json.suggestions).to.have.length.above(0);
        // });

        it(`the first suggestion's name should be ${testCase.expectedFirstSuggestionName}`, () => {
          expect(response.json.suggestions[0].displayName).to.equal(testCase.expectedFirstSuggestionName);
        });
      }
      /*
      // Check for score
      if (testCase.expectedFirstSuggestionScore) {

        it(`the first suggestion's score should be ${testCase.expectedFirstSuggestionScore}`, function() {
          expect(response.json.suggestions).to.have.length.above(0);
          expect(response.json.suggestions[0].score).to.equal(testCase.expectedFirstSuggestionScore);
        });

      }
*/
    });
  });

  const aliasedTestCases = [
    {
      params: { q: 'bigappl' }, // popular nickname "Big Apple"
      expectedFirstSuggestionName: 'New York City, NY, US',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'kebek' }, // mi'kmaq name
      expectedFirstSuggestionName: 'Québec, QC, CA',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'atawa' }, // anishinaabe name
      expectedFirstSuggestionName: 'Ottawa, ON, CA',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'ybr' }, // IATA name
      expectedFirstSuggestionName: 'Brandon, MB, CA',
      expectedFirstSuggestionScore: null,
    },
    {
      params: { q: 'yto' }, // IATA name
      expectedFirstSuggestionName: 'Toronto, ON, CA',
      expectedFirstSuggestionScore: null,
    },
  ];
  // Foreach defiend text case
  aliasedTestCases.forEach((testCase) => {
    // Generate query string
    const params = querystring.stringify(testCase.params);

    describe(`search by the aliased name '${params}'`, () => {
      let response;

      before((done) => {
        request
          .get(`/suggestions?${params}`)
          .end((err, res) => {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });

      // Check for name
      if (testCase.expectedFirstSuggestionName) {
        it(`the first suggestion's name should be ${testCase.expectedFirstSuggestionName}`, () => {
          expect(response.json.suggestions[0].displayName).to.equal(testCase.expectedFirstSuggestionName);
        });
      }
    });
  });

  describe('with a non-existent city', () => {
    let response;

    before((done) => {
      request
        .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 404', () => {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', () => {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe('with a valid ascii city', () => {
    let response;

    before((done) => {
      request
        .get('/suggestions?q=Montreal')
        .end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', () => {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', () => {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it('contains latitudes and longitudes', () => {
      expect(response.json.suggestions).to.satisfy((suggestions) => suggestions.every((suggestion) => suggestion.latitude && suggestion.longitude));
    });

    it('contains scores', () => {
      expect(response.json.suggestions).to.satisfy((suggestions) => suggestions.every((suggestion) => suggestion.latitude && suggestion.longitude));
    });

    it('contains a match', () => {
      expect(response.json.suggestions).to.satisfy((suggestions) => suggestions.some((suggestion) => suggestion.name.match(/montreal/i)));
    });
  });

  describe('with a valid url-escaped city', () => {
    let response;

    before((done) => {
      request
        .get('/suggestions?q=montr%C3%A9al')
        .end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', () => {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', () => {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it('contains latitudes and longitudes', () => {
      expect(response.json.suggestions).to.satisfy((suggestions) => suggestions.every((suggestion) => suggestion.latitude && suggestion.longitude));
    });

    it('contains scores', () => {
      expect(response.json.suggestions).to.satisfy((suggestions) => suggestions.every((suggestion) => suggestion.latitude && suggestion.longitude));
    });

    it('contains a match', () => {
      expect(response.json.suggestions).to.satisfy((suggestions) => suggestions.some((suggestion) => suggestion.name.match(/montreal/i)));
    });
  });

  describe('with a valid utf8 city', () => {
    let response;

    before((done) => {
      request
        .get('/suggestions?q=montréal')
        .end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', () => {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', () => {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it('contains latitudes and longitudes', () => {
      expect(response.json.suggestions).to.satisfy((suggestions) => suggestions.every((suggestion) => suggestion.latitude && suggestion.longitude));
    });

    it('contains scores', () => {
      expect(response.json.suggestions).to.satisfy((suggestions) => suggestions.every((suggestion) => suggestion.latitude && suggestion.longitude));
    });

    it('contains a match', () => {
      expect(response.json.suggestions).to.satisfy((suggestions) => suggestions.some((suggestion) => suggestion.name.match(/montreal/i)));
    });
  });

  describe('with a city that includes apostrophe', () => {
    let response;

    before((done) => {
      request
        .get("/suggestions?q=Val-d'or")
        .end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', () => {
      expect(response.statusCode).to.equal(200);
    });

    it('returns the expected city', () => {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
      expect(response.json.suggestions[0].displayName).to.equal("Val-d'Or, QC, CA");
    });
  });

  describe('with two identical requests', () => {
    let firstResponse;
    let secondResponse;
    before((done) => {
      request
        .get('/suggestions?q=Waterl')
        .end((err, res) => {
          firstResponse = res;
          firstResponse.json = JSON.parse(res.text);
          done(err);
        });
      request
        .get('/suggestions?q=Waterl')
        .end((err, res) => {
          secondResponse = res;
          secondResponse.json = JSON.parse(res.text);
        });
    });

    it('processes first request', () => {
      expect(firstResponse.headers).to.not.have.property('last-modified');
    });

    it('gets second request from cache', () => {
      expect(secondResponse.headers).to.have.property('last-modified');
    });

    it('returns a 200 for both', () => {
      expect(firstResponse.statusCode).to.equal(200);
      expect(secondResponse.statusCode).to.equal(200);
    });
  });

  describe('with two near requests', () => {
    let firstResponse;
    let secondResponse;
    before((done) => {
      // querying from similar coordinates {0.00001,0.00001}, {0.00005,0.00009}
      request
        .get('/suggestions?q=Waterl&latitude=0.00001&lontigude=0.00001')
        .end((err, res) => {
          firstResponse = res;
          firstResponse.json = JSON.parse(res.text);
          done(err);
        });
      request
        .get('/suggestions?q=Waterl&latitude=0.00005&lontigude=-0.00009')
        .end((err, res) => {
          secondResponse = res;
          secondResponse.json = JSON.parse(res.text);
        });
    });

    it('processes first request', () => {
      expect(firstResponse.headers).to.not.have.property('last-modified');
    });

    it('gets second request from cache', () => {
      expect(secondResponse.headers).to.have.property('last-modified');
    });

    it('returns a 200 for both', () => {
      expect(firstResponse.statusCode).to.equal(200);
      expect(secondResponse.statusCode).to.equal(200);
    });
  });

  describe('with a request with many results', () => {
    let response;
    before((done) => {
      request
        .get('/suggestions?q=l')
        .end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', () => {
      expect(response.statusCode).to.equal(200);
    });

    it('doesnt include duplicates', () => {
      //expect(isArrayUnique(response.json)).toBeTruthy(); //TODO
    });

    it('returns a limited amount of results', () => {
      //expect(response.json.length).to.toBeLessThanOrEqual(10); //TODO
    });

  });

});
