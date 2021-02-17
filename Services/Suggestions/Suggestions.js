const { parseDataSource, findObjects, sortResults } = require('../../helpers');

const simpleCache = {};
module.exports.index = async (request, response) => {
  const params = request.query;
  let sortedResults = [];
  let dataToSort = [];
  try {
    if (simpleCache[params.q] !== undefined) {
      dataToSort = simpleCache[params.q];
    } else {
      const result = await parseDataSource();
      dataToSort = findObjects(params.q, result);
      simpleCache[params.q] = dataToSort;
    }

    let coordinateQuery =
      params.latitude && params.longitude
        ? {
            coordinates: { lat: params.latitude, lon: params.longitude },
          }
        : params.q;

    sortedResults = await sortResults(coordinateQuery, dataToSort);

    response.send({
      suggestions: sortedResults,
    });
  } catch (e) {
    console.log(e);
  }
};
