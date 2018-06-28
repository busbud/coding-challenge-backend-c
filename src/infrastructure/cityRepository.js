const fs = require("fs");
const es = require("event-stream");
const { filter } = require("fuzzaldrin");

module.exports = ({ dbFile = null } = {}) => {
  const MIN_POPULATION = 5000;

  const toJson = fields => {
    return {
      name: fields[1],
      nameAscii: fields[2],
      location: {
        longitude: parseFloat(fields[5]),
        latitude: parseFloat(fields[4])
      },
      state: isNaN(fields[10]) ? fields[10] : "",
      country: fields[8],
      population: parseInt(fields[14], 10)
    };
  };

  const filterMinPopulation = city => {
    if (city.population > MIN_POPULATION) {
      return city;
    }
  };

  const filterByName = (name, city) => {
    let candidates = [city.name, city.nameAscii];
    let results = filter(candidates, name);
    if (results.length > 0) {
      return city;
    }
  };

  const fetchCities = () =>
    fs
      .createReadStream(dbFile)
      .pipe(es.split("\n"))
      .pipe(es.mapSync(data => data.split("\t")))
      .pipe(es.mapSync(toJson))
      .pipe(es.mapSync(filterMinPopulation));

  return {
    findByName: async name =>
      new Promise((resolve, reject) => {
        let boundedFilterByName = filterByName.bind(this, name);
        let writer = es.writeArray(function(err, array) {});
        fetchCities()
          .pipe(es.mapSync(boundedFilterByName))
          .pipe(
            es.writeArray((err, result) => {
              if (err) {
                return resolve([]);
              }

              return resolve(result);
            })
          );
      })
  };
};
