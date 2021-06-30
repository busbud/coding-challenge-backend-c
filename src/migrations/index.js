/* TODO: Need improvement */
const fs = require('fs')
const path = require('path')

module.exports = async (searchEngineClient) => {
  const files = fs.readdirSync(__dirname)
  for (const file of files) {
    if (file.match(/\.js$/) !== null && file !== 'index.js') {
      const migration = require(path.join(__dirname, file))
      await searchEngineClient.applyMigration(migration)
    }
  }
}
