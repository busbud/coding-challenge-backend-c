const findSuggestions = require('./findSuggestions')

module.exports = (searchEngineClient) => ({
  findSuggestions: findSuggestions(searchEngineClient)
})
