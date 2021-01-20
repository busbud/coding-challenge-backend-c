const {index, client} = require('./elasticsearch.client')
const fs = require('fs');

const deleteIndex = async () => await client.indices.delete({index: index})
    .catch(reason => console.log(reason))

const createIndex = async (source) => {
    const indexBody = JSON.parse(fs.readFileSync(source, 'utf8'))
    await client.indices.create({
        index: index,
        body: indexBody
    })
}

module.exports.deleteIndex = deleteIndex
module.exports.createIndex = createIndex
