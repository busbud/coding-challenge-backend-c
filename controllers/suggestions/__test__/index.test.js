const suggestions = require('../');
const config = require('../../../config').get();

// This is a mock of the es.Client instance
function getClient(createCode = 200, existsCode = 200) {
  return {};
}

function getDb() {
  return {
    search: function () {
      let data = require('./data')
        .map((val) => {
          return { _source: val };
        });

      return {
        body: {
          suggest: {
            asciiSuggester: [
              {
                options: data
              }
            ]
          }
        }
      };
    }
  };
}

describe('controllers.suggestions', () => {
  let client;
  let controller;

  beforeEach(() => {
    client = getClient();
    db = getDb();
    controller = suggestions(client, db, config);
  });

  describe('getDistanceScores()', () => {
    test('should return right values', () => {
      const dummyCoords = { latitude: '43.70011', longitude: '-79.4163' };
      const dummyList = require('./data');
      const expected = {
        '4298960': 0,
        '4361094': 0.375,
        '4517009': 0.4382530120481928,
        '5088905': 0.2605421686746988,
        '6058560': 1
      };

      const result = controller.getDistanceScores(dummyList, dummyCoords);
      expect(result).toEqual(expected);
    });
  });

  describe('get()', () => {
    test('should return expected list [not passing coords]', async () => {
      let result = await controller.get('londo', null);
      let expected = [
        {
          name: 'London, Ontario, Canada',
          latitude: 42.98339,
          longitude: -81.23304,
          score: 0.9
        },
        {
          name: 'London, KY, United States',
          latitude: 37.12898,
          longitude: -84.08326,
          score: 0.9
        },
        {
          name: 'London, OH, United States',
          latitude: 39.88645,
          longitude: -83.44825,
          score: 0.9
        },
        {
          name: 'Londonderry, NH, United States',
          latitude: 42.86509,
          longitude: -71.37395,
          score: 0.6
        },
        {
          name: 'Londontowne, MD, United States',
          latitude: 38.93345,
          longitude: -76.54941,
          score: 0.6
        }
      ];

      expect(result).toEqual(expected);
    });

    test('should return expected list [passing coords]', async () => {
      const dummyCoords = { latitude: '43.70011', longitude: '-79.4163' };
      let result = await controller.get('londo', dummyCoords);

      let expected = [{
        name: 'London, Ontario, Canada',
        latitude: 42.98339,
        longitude: -81.23304,
        score: 0.9
      },
      {
        name: 'London, OH, United States',
        latitude: 39.88645,
        longitude: -83.44825,
        score: 0.8
      },
      {
        name: 'London, KY, United States',
        latitude: 37.12898,
        longitude: -84.08326,
        score: 0.6
      },
      {
        name: 'Londonderry, NH, United States',
        latitude: 42.86509,
        longitude: -71.37395,
        score: 0.5
      },
      {
        name: 'Londontowne, MD, United States',
        latitude: 38.93345,
        longitude: -76.54941,
        score: 0.5
      }];

      expect(result).toEqual(expected);
    });
  });
});