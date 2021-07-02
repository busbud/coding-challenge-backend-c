module.exports = async function (request, reply) {
  const { query } = request

  const cacheKey = JSON.stringify(query)

  const suggestions = await this.cacheManager.wrap(cacheKey, async () => {
    const repository = this.searchEngineClient.getRepository('suggestions')
    return repository.findSuggestions(query.q, query)
  })

  let statusCode = 200

  if (suggestions.length === 0) {
    statusCode = 404
  }

  reply.status(statusCode).send({ suggestions })
}
