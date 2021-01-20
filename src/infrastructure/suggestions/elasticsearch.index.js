const {index, client} = require('./elasticsearch.client')

const deleteIndex = async () => await client.indices.delete({index: index})
    .catch(reason => console.log(reason))


module.exports.deleteIndex = deleteIndex
