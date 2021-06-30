module.exports = async (client) => {
  const { body: exists } = await client.indices.exists({ index: 'cities' })
  if (!exists) {
    const options = {
      index: 'cities',
      body: {
        settings: {
          analysis: {
            analyzer: {
              folding: {
                tokenizer: 'standard',
                filter: [
                  'lowercase',
                  'asciifolding'
                ]
              }
            }
          }
        },
        mappings: {
          properties: {
            id: { type: 'integer' },
            name: { type: 'text' },
            country: { type: 'text' },
            region: { type: 'text' },
            location: { type: 'geo_point' },
            displayName: {
              type: 'search_as_you_type',
              analyzer: 'folding',
              search_analyzer: 'folding'
            }
          }
        }
      }
    }

    return client.indices.create(options)
  }
}
