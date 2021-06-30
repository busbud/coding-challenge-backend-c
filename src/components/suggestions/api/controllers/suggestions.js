module.exports = async function (request, reply) {
  const { query } = request

  const cacheKey = JSON.stringify(query)
  return this.cacheManager.wrap(cacheKey, async () => {
    const repository = this.searchEngineClient.getRepository('suggestions')
    const suggestions = await repository.findSuggestions(query.q, query)
    return {
      suggestions
    }
  })
}
