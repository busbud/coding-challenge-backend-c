const formatter = require('../formatter')

/**
 * @function
 * @description Initialize the repository
 * @param {SearchEngineClient} searchEngineClient
 * @returns {function}
 */
module.exports = (searchEngineClient) => {
  /**
   * @function
   * @async
   * @description Find the city suggestions
   * @param {string} name
   * @param {Object} [options]
   * @param {number} [options.latitude]
   * @param {number} [options.longitude]
   * @param {string} [options.radius='200km']
   * @param {number} [options.limit=5]
   * @param {number} [options.minScore= 0.0001]
   * @returns {function}
   */
  return async (
    name,
    {
      latitude,
      longitude,
      radius = '200km',
      limit = 5,
      minScore = 0.0001
    } = {}) => {
    const functions = []
    if (latitude && longitude) {
      functions.push({
        gauss: {
          location: {
            origin: {
              lat: parseFloat(latitude),
              lon: parseFloat(longitude)
            },
            scale: radius
          }
        }
      })
    }

    const criteria = {
      query: {
        function_score: {
          query: {
            match_phrase_prefix: {
              displayName: {
                query: name
              }
            }
          },
          functions
        }
      },
      size: limit,
      min_score: minScore
    }

    const { body } = await searchEngineClient.search({
      index: 'cities',
      body: criteria
    })

    return formatter(body)
  }
}
