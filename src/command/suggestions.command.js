const saveAdapter = require('../infrastructure/suggestions/save.adapter');
const geonamesImporter = require('../infrastructure/geoname.importer.adapter');

const createOrUpdate = (suggestion) => new Promise((resolve, reject) => {
  saveAdapter.save(suggestion)
    .then((saved) => resolve(saved))
    .catch((reason) => reject(reason));
});

const importFile = async (source) => {
  await geonamesImporter(source, createOrUpdate);
};

module.exports.createOrUpdate = createOrUpdate;
module.exports.importFile = importFile;
