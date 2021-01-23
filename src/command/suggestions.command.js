const saveAdapter = require('../infrastructure/suggestions/save.adapter');
const geonameImporter = require('../infrastructure/geoname.importer.adapter');

const createOrUpdate = (suggestion) => new Promise((resolve, reject) => {
  saveAdapter.save(suggestion)
    .then((saved) => resolve(saved))
    .catch((reason) => reject(reason));
});

const importFile = async (source) => {
  await geonameImporter(source, createOrUpdate);
};

module.exports.createOrUpdate = createOrUpdate;
module.exports.importFile = importFile;
