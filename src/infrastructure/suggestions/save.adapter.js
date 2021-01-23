const { index, client } = require('./elasticsearch.client');

const save = (suggestion) => new Promise((resolve, reject) => {
  const suggestionEntity = JSON.parse(JSON.stringify(suggestion));
  delete suggestionEntity.id;
  client.index({
    id: suggestion.id,
    index,
    body: suggestionEntity,
    refresh: true,
  }).then(() => resolve(suggestion))
    .catch((reason) => reject(reason));
});

exports.save = save;
