/**
 * returns an instance of the elasticsearch client
 * @param {Object} config configuration dictionary
 * @param {elasticsearch} es elasticsearch package
 * @returns {elasticsearch.Client} instantiated client
 */
module.exports.getClient = async function (config, es) {
  let client;

  client = await new es.Client({
    node: config.url,
    maxRetries: 5,
    requestTimeout: 60000,
  });
  await client.ping();

  return client;
};

/**
 * create an index if it doesn't exist
 * @param {String} index index name
 * @param {elasticsearch.Client} client es client
 * @returns {Object} response returned by the es client
 */
module.exports.initIndex = async function (index, client) {
  let response;

  response = await client.indices.exists({index});

  if (response.statusCode === 200) {
    return response;
  }

  return await client.indices.create({index});
};

/**
 * Puts a mapping to an index in ES
 * @param {Object} mapping ES compatible mapping
 * @param {String} index index name
 * @param {elasticsearch.Client} client es client
 * @returns {Promise} that resolves to client response
 */
module.exports.putMapping = function (mapping, index, type, client) {
  return client.indices.putMapping({
    type: type,
    include_type_name: true,
    index: index,
    body: {
      properties: mapping
    }
  });
};

/**
 * Uploads a set of documents to the es db
 * @param {Object} body ES compatible bulk query
 * @param {String} index index name
 * @param {elasticsearch.Client} client es client
 * @returns {Promise} that resolves to client response
 */
module.exports.bulkUpload = function(body, index, client) {
  return client.bulk({
    index: index,
    body: body
  });
};

/**
 * Uploads a set of documents to the es db
 * @param {elasticsearch.Client} client es client
 * @param {String} index index name
*  @param {Object} query ES compatible search query
 * @returns {Promise} that resolves to client response
 */
module.exports.search = function (client, index, query) {
  return client.search({
    index: index,
    body: query
  });
};