/**
* Schema for cities index
*/
class CitiesSchema {

  constructor(client) {
    this.client = client;
  }

  open() {
    return this.client.indices.open({index: 'cities'});
  }

  close() {
    return this.client.indices.close({index: 'cities'});
  }

  createIfNotExists() {
    return this.client.indices.exists({
      index: 'cities'
    }).then((exists) => {
      if (!exists) {
        return this.client.indices.create({
          index: 'cities'
        });
      }
    });
  }

  putSettings() {
    return this.client.indices.putSettings({
      index: 'cities',
      body: {
        analysis: {
          analyzer: {
            'city_search_analyzer': {
              type: 'custom',
              tokenizer: 'standard',
              filter: [ 'standard', 'lowercase', 'asciifolding']
            },
            'city_index_analyzer': {
              type: 'custom',
              tokenizer: 'standard',
              filter: [ 'standard', 'lowercase', 'asciifolding', 'custom_n_gram_filter']
            }
          },
          tokenizer: {
            'custom_n_gram_tokenizer': {
              type: 'edgeNGram',
              'token_chars': [ 'letter', 'digit', 'whitespace' ],
              'min_gram': 1,
              'max_gram': 20
            }
          },
          'filter':{
            'custom_n_gram_filter':{
              'type':'edgeNGram',
              'min_gram': 1,
              'max_gram': 20,
              'side': 'front'
            }
          }
        }
      }
    });
  }

  putMapping() {
    return this.client.indices.putMapping({
      index: 'cities',
      type: 'city',
      body: {
        properties: {
          name: {
            type: 'string',
            analyzer: 'city_search_analyzer',
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
    });
  }

  load() {
    return this.createIfNotExists().then(() => {
      return this.close();
    }).then(() => {
      return this.putSettings();
    }).then(() => {
      return this.putMapping();
    }).then(() => {
      return this.open();
    });
  }
}

export default CitiesSchema;
