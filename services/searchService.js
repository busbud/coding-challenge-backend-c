const queryString = require("query-string");
const getCities = require("./cityService");

async function search(queryUrl) {
  const { q: query } = queryString.parse(queryUrl);

  let result = [];

  if (query) {
    result = await getCities();
    result = searchByQuery(query, result);

    if (result && result.length > 0) {
      result = searchByCoordinate(queryUrl, "latitude", result);
      result = searchByCoordinate(queryUrl, "longitude", result);
    }
  }
  computeScore(result);
  cleanResult(result);

  return result;
}

function searchByQuery(query, result) {
  result = result.filter(item => {
    query = query.toLowerCase();
    const name = item.name.toLowerCase();
    const alternatenames = item.alternatenames.toLowerCase();

    let match = name.includes(query);

    if (!match) {
      match = alternatenames.includes(query);
    }
    return match;
  });
  return result;
}

function searchByCoordinate(queryUrl, coordinate, result) {
  const searchObj = queryString.parse(queryUrl);
  const searchValue = searchObj[coordinate];

  if (searchValue) {
    result = result.filter(city => {
      return city[coordinate].includes(searchValue);
    });
  }
  return result;
}

function cleanResult(result) {
  result.map(item => {
    item.name = item.name + ", " + item.adminCode + ", " + item.countryCode;
    delete item.alternatenames;
    delete item.adminCode;
    delete item.countryCode;
  });
}

function computeScore(result) {
  result.map(item => (item.score = 1));
}

module.exports = search;
