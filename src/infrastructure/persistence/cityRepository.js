const fs = require("fs");
const es = require("event-stream");
const { filter } = require("fuzzaldrin");
const geolib = require("geolib");
const { scoreName, scoreDistance, scoring, sort } = require("../score");
const { toCity } = require("./cityMapper");

const MIN_POPULATION = 5000;
const METERS_IN_KM = 1000;
const PRECISION_HUNDRED_METERS = 100;

const filterByName = (name, city) => {
  let candidates = [city.name, city.nameAscii];
  let results = filter(candidates, name);
  return results.length > 0;
};

const filterByDistance = (radiusInKm = 100, city) => city.distance <= radiusInKm * METERS_IN_KM;

const computeDistance = (location = { longitude: null, latitude: null }, radiusInKm = 100, city) => {
  let distanceInMeters = geolib.getDistance(location, city.location, PRECISION_HUNDRED_METERS);
  return { ...city, distance: distanceInMeters };
};

const scoringName = (name, city) => {
  return { ...city, scoringName: scoreName(city.name, name) };
};

const scoringDistance = (radiusInKm, city) => {
  return { ...city, scoringDistance: scoreDistance(city.distance, radiusInKm) };
};

const computeScore = city => {
  return { ...city, score: scoring({ scoringName: city.scoringName, scoringDistance: city.scoringDistance }) };
};

const filterMinPopulation = city => {
  if (city.population > MIN_POPULATION) {
    return city;
  }
};

const loadCities = dbFile =>
  fs
    .createReadStream(dbFile)
    .pipe(es.split("\n"))
    .pipe(es.mapSync(data => data.split("\t")))
    .pipe(es.mapSync(toCity))
    .pipe(es.mapSync(filterMinPopulation));

module.exports = class CityRepository {
  constructor(dbFile) {
    this.dbFile = dbFile;
    this.cities = [];
    loadCities(dbFile).pipe(
      es.writeArray((err, data) => {
        this.cities = data;
      })
    );
  }

  findByName(name) {
    let boundedFilterByName = filterByName.bind(this, name);
    let boundedScoringName = scoringName.bind(this, name);

    return Promise.resolve(
      this.cities
        .filter(boundedFilterByName)
        .map(boundedScoringName)
        .map(computeScore)
        .sort(sort)
        .slice(0, 20)
    );
  }

  findByNameAndLocation(name, location = { longitude: null, latitude: null }, radiusInKm = 100) {
    let boundedFilterByName = filterByName.bind(this, name);
    let boundedComputeDistance = computeDistance.bind(this, location, radiusInKm);
    let boundedFilterByDistance = filterByDistance.bind(this, radiusInKm);
    let boundedScoringName = scoringName.bind(this, name);
    let boundedScoringDistance = scoringDistance.bind(this, radiusInKm);

    return Promise.resolve(
      this.cities
        .filter(boundedFilterByName)
        .map(boundedComputeDistance)
        .filter(boundedFilterByDistance)
        .map(boundedScoringName)
        .map(boundedScoringDistance)
        .map(computeScore)
        .sort(sort)
        .slice(0, 20)
    );
  }
};
