module.exports = async function (request) {
  const searchEngineHealth = await this.searchEngineClient.healthCheck()
  const cacheManagerHealth = await this.cacheManager.healthCheck()

  return { searchEngine: searchEngineHealth, cacheManager: cacheManagerHealth }
}
