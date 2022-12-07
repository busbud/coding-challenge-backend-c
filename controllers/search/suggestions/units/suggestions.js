const { tsvToJson, getScore, getNameScore } = require("../../../../lib");
const countries = require("../../../../constants/countries");
const provinces = require("../../../../constants/provinces");
module.exports = async ({ q, location }) => {
  // gets json data
  const data = tsvToJson();

  return {
    suggestions: data
      .filter(
        (item) =>
          ["CA", "US"].includes(item.country) &&
          Number(item.population) > 5000 &&
          item.name &&
          item.name
            .toLowerCase()
            .normalize("NFD")
            .replace(/[\u0300-\u036f]/g, "")
            .includes(
              q
                .toLowerCase()
                .normalize("NFD")
                .replace(/[\u0300-\u036f]/g, "")
            )
      )
      .map(({ name, admin1, country, lat, long }) => ({
        name: `${name}, ${
          Number(admin1) ? provinces[country][Number(admin1)] : admin1
        }, ${countries[country]}`,
        latitude: lat,
        longitude: long,
        score: getScore(name, q, location, {
          latitude: lat,
          longitude: long,
        }),
      })),
  };
};
