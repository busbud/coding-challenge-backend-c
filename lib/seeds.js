const Suggestion = require("../src/services/SuggestionService");
const Promise = require("bluebird");
const csv = require("csvtojson");

/**
 * @method tsvToJSON
 * @description converts tsv data to json array using `csvtojson` module
 * @param {{inputPath}} options
 * @returns {Array}
 */
const tsvToJSON = async ({ inputPath }) => {
  const options = {
    delimiter: "\t",
    columns: true,
    quote: false,
  };

  const jsonArray = await csv(options).fromFile(inputPath);

  return jsonArray.map((obj) => {
    return {
      id: obj.id,
      name: `${obj.ascii}, ${obj.admin1}, ${obj.country}`,
      latitude: obj.lat,
      longitude: obj.long,
      population: obj.population,
    };
  });
};

/**
 * @method migrateToDB
 * @description migrates the data to db via service call
 * @param {{data}} options
 * @returns {Promise}
 */
const migrateToDB = async ({ data }) => {
  const cities = data;
  return await Promise.map(
    cities,
    (city) => {
      Suggestion.add(city);
    },
    { concurrency: 20 }
  );
};

module.exports = { migrateToDB, tsvToJSON };
