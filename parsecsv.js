const fs = require("fs");
const _ = require("lodash");

const parsecsv = (filepath) => {
  const [colnames, ...records] = fs.readFileSync(filepath, "utf-8")
    .trim().split("\n")
    .map(line => line.split("\t"))

  fips_code_canada = [
    "", "AB", "BC", "MB", "NB", "NL", "", "NS", "ON", "PE", "QC", "SK", "YT", "NT", "NU"
  ];

  return _.chain(records)
    .map(r => _.pick(_.zipObject(colnames, r), [
      "ascii", "admin1", "country", "lat", "long", "population"
    ]))
    .filter(({ country }) => country === "US" || country === "CA")
    .map(r => {
      admin1 = Number(r["admin1"])? fips_code_canada[Number(r["admin1"])] : r["admin1"];
      return {
        latitude: Number(r["lat"]),
        longitude: Number(r["long"]),
        population: Number(r["population"]),
        name: `${r["ascii"]}, ${admin1}, ${r["country"]}`
      };
    })
    .filter(({ population }) => population >= 5000)
    .value();
};

module.exports = { parsecsv };
