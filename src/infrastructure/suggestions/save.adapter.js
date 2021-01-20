const {index, client} = require('./elasticsearch.client')

const save = (suggestion) => new Promise(async (resolve, reject) => {
    let suggestionEntity = JSON.parse(JSON.stringify(suggestion));
    delete suggestionEntity['id']
    await client.index({
        id: suggestion.id,
        index: index,
        body: suggestionEntity,
        refresh: true
    }).catch(reason => reject(reason))
    await client.indices.refresh({index: index})
    resolve(suggestion)
})

exports.save = save
