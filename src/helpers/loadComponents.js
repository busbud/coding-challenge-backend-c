const deepmerge = require('deepmerge')

/**
 * @function
 * @description Load the components
 * @param {Object} components
 * @returns {Object}
 */
module.exports = (components) => {
  const routers = []
  const repositories = []
  let apiDocs = {}
  for (const [name, component] of Object.entries(components)) {
    if (component.api && component.api.routes) {
      routers.push({ name, routes: component.api.routes })
    }
    if (component.api && component.api.documentation) {
      apiDocs = deepmerge(apiDocs, component.api.documentation)
    }

    if (component.searchEngine && component.searchEngine.repository) {
      repositories.push({ name, repository: component.searchEngine.repository })
    }
  }

  return {
    apiDocs,
    repositories,
    routers
  }
}
