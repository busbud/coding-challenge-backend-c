const fs = require("fs");
const es = require("event-stream");
const { filter } = require("fuzzaldrin");
const score = require("string-score");
const geolib = require("geolib");
const { scoreName, scoreDistance, scoring } = require("../score");
const { toObj } = require("./cityMapper");

module.exports = ({ dbFile = null } = {}) => {
  const MIN_POPULATION = 5000;
  const METERS_IN_KM = 1000;
  const PRECISION_HUNDRED_METERS = 100;

  const fetchCities = () =>
    fs
      .createReadStream(dbFile)
      .pipe(es.split("\n"))
      .pipe(es.mapSync(data => data.split("\t")))
      .pipe(es.mapSync(toObj))
      .pipe(es.mapSync(filterMinPopulation));

  const filterMinPopulation = city => {
    if (city.population > MIN_POPULATION) {
      return city;
    }
  };

  const filterByName = (name, city) => {
    let candidates = [city.name, city.nameAscii];
    let results = filter(candidates, name);
    if (results.length > 0) {
      return { ...city, scoringName: scoreName(city.name, name) };
    }
  };

  const filterByDistance = (location = { longitude: null, latitude: null }, radiusInKm = 100, city) => {
    let distanceInMeters = geolib.getDistance(location, city.location, PRECISION_HUNDRED_METERS);
    if (distanceInMeters <= radiusInKm * METERS_IN_KM) {
      return { ...city, scoringDistance: scoreDistance(distanceInMeters, radiusInKm) };
    }
  };

  const computeScore = city => ({ ...city, score: scoring(({ scoringName, scoringDistance } = city)) });

  return {
    findByName: name =>
      new Promise((resolve, reject) => {
        let boundedFilterByName = filterByName.bind(this, name);
        fetchCities()
          .pipe(es.mapSync(boundedFilterByName))
          .pipe(es.mapSync(computeScore))
          .pipe(
            es.writeArray((err, result) => {
              if (err) {
                return resolve([]);
              }

              return resolve(result);
            })
          );
      }),

    findByNameAndLocation: (name, location = { longitude: null, latitude: null }, radiusInKm = 100) =>
      new Promise((resolve, reject) => {
        let boundedFilterByName = filterByName.bind(this, name);
        let boundedFilterByDistance = filterByDistance.bind(this, location, radiusInKm);
        return fetchCities()
          .pipe(es.mapSync(boundedFilterByName))
          .pipe(es.mapSync(boundedFilterByDistance))
          .pipe(es.mapSync(computeScore))
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
