const fs = require('fs');
const { index, client } = require('./elasticsearch.client');

const deleteIndex = async () => client.indices.delete({ index })
  .catch((reason) => console.log(reason));

const createIndex = async (source) => {
  const indexBody = JSON.parse(fs.readFileSync(source, 'utf8'));
  await client.indices.create({
    index,
    body: indexBody,
  }).catch((reason) => console.log(reason));
};

module.exports.deleteIndex = deleteIndex;
module.exports.createIndex = createIndex;
