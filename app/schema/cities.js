/**
* Schema for cities index
*/
class CitiesSchema {

  constructor(client) {
    this.client = client;
  }

  createIfNotExists() {
    return this.client.indices.exists({
      index: 'cities'
    }).then((exists) => {
      if (!exists) {
        return this.client.indices.create({
          index: 'cities',
          body: {
            mappings: {
              city: {
                properties: {
                  name: {
                    type: 'string',
                    search_analyzer: 'city_search_analyzer',
                    index_analyzer: 'city_index_analyzer'
                  },
                  population: {
                    type: 'integer'
                  },
                  location: {
                    type: 'geo_point'
                  }
                }
              }
            },
            settings: {
              analysis: {
                analyzer: {
                  'city_search_analyzer': {
                    type: 'custom',
                    tokenizer: 'standard',
                    filter: ['standard', 'lowercase', 'asciifolding']
                  },
                  'city_index_analyzer': {
                    type: 'custom',
                    tokenizer: 'standard',
                    filter: ['standard', 'lowercase', 'asciifolding', 'custom_n_gram_filter']
                  }
                },
                filter:{
                  'custom_n_gram_filter':{
                    'type':'edgeNGram',
                    'min_gram': 1,
                    'max_gram': 20,
                    'side': 'front'
                  }
                }
              }
            }
          }
        });
      }
    });
  }
}

export default CitiesSchema;
