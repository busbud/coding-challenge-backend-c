const NodeCache = require("node-cache");
const searchCache = new NodeCache({ stdTTL: 30 });
const search = require("./searchService");

function getCache(url) {
  const queryUrl = url.substring(url.indexOf("?"), url.length);

  return new Promise((resolve, reject) => {
    searchCache.get(queryUrl, function(err, result) {
      if (!err) {
        if (!result) {
          resolve(getData(queryUrl));
        } else {
          console.log("data retrieved from cache");
          resolve(result);
        }
      } else {
        reject(err);
      }
    });
  });
}

async function getData(queryUrl) {
  const result = await search(queryUrl);

  if (hasData(result)) {
    searchCache.set(queryUrl, result, function(err, success) {
      if (!err && success) console.log("data cached");
    });
  }
  return result;
}

function hasData(data) {
  return data && data.length > 0;
}

module.exports = { getCache, hasData };
