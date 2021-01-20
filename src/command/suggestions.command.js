const saveAdapter = require('../infrastructure/suggestions/save.adapter')
const geonameImporter = require('../infrastructure/geoname.importer.adapter')

const createOrUpdate = async (suggestion) => new Promise(async (resolve, reject) => {
    const saved = await saveAdapter.save(suggestion)
        .catch(reason => reject(reason))
    resolve(saved)
})

const importFile = async (source) => {
    await geonameImporter(source, createOrUpdate)
}

module.exports.createOrUpdate = createOrUpdate
module.exports.importFile = importFile
