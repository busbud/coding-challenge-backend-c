module.exports = async function (request, reply) {
  const { query } = request

  const cacheKey = JSON.stringify(query)
  return this.cacheManager.wrap(cacheKey, async () => {
    let statusCode = 200
    const repository = this.searchEngineClient.getRepository('suggestions')
    const suggestions = await repository.findSuggestions(query.q, query)

    if (suggestions.length === 0) {
      statusCode = 404
    }

    reply.status(statusCode).send({ suggestions })
  })
}
