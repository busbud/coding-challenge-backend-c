/**
* Data store to query cities index
*/
class DataStore {
  /**
  * Constructor
  *
  * @param {object} ElasticSearch client
  */
  constructor(client) {
    this.client = client;
  }

  /**
  * Search
  *
  * @param {string} query string
  * @param {object} optionnal latitude and longitude {lat, lng}
  * @return {Promise}
  */
  search(query, geo = null ) {

    var relevanceFunctions = [
      {
        'field_value_factor': {
          field: 'population',
          modifier: 'log1p',
          factor: 0.001
        },
        weight: 4
      }
    ];

    if (geo) {
      relevanceFunctions.push({
        gauss: { location:
          { origin: geo.lat.toString() + ',' + geo.lng.toString() , scale: '40km', decay: 0.1}
        },
        weight: 5
      });
    }

    return this.client.search({
      index: 'cities',
      type: 'city',
      from : 0,
      size : 10,
      body: {
        query: {
          'function_score': {
            query: {
              match: {
                name: query
              },
            },
            functions: relevanceFunctions,
            'score_mode': 'sum'
          }
        },
        explain: true
      }
    }).then(function (resp) {
      return resp.hits.hits;
    });
  }
}

export default DataStore;
