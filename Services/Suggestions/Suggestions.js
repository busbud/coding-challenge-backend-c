const { parseDataSource, findObjects, sortResults } = require('../../helpers');

module.exports.index = async (request, response) => {
  const params = request.query;
  let sortedResults = [];
  try {
    const result = await parseDataSource();
    const dataTo = findObjects(params.q, result);
    let coordinateQuery = {};
    if (params.latitude && params.longitude) {
      coordinateQuery = {
        coordinates: { lat: params.latitude, lon: params.longitude },
      };
    } else {
      coordinateQuery = params.q;
    }
    sortedResults = await sortResults(coordinateQuery, dataTo);

    response.send({
      suggestions: sortedResults,
    });
  } catch (e) {
    console.log(e);
  }
};
