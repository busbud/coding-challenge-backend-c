var QueryModel = function(url_query) {
  return {
    search: url_query.q,
    latitude: url_query.latitude || 0,
    longitude: url_query.longitude || 0
  };
};

module.exports = {
  make: makeQuery
};

function makeQuery(url_query) {
  return QueryModel(url_query);
}
