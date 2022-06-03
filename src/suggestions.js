import { tsvToJson } from "./utils/dataParser.js";
import { compareTwoStrings } from "string-similarity";
import * as geodistance from "geo-distance";

const tsvFile = "data/cities_canada-usa.tsv";

let jsonData;

// Use an arbitrary distance of 500
// We could do some more complicated math, but this is a nice simple solution
const distanceMax = 500;

// Because this is an autocomplete, I opted to use startsWith to narrow results
// If we wanted to let users type something like "ttawa" and match "Ottawa",
// we could change that here
const findCities = (input, data) => {
  return data.filter(
    (city) =>
      city.name && city.name.toLowerCase().startsWith(input.toLowerCase())
  );
};

// Calculate the relative equalness of the two strings using a library
const calculateScore = (input, data) => {
  return data.map((city) => {
    let newCity = {
      name: `${city.name}, ${city.state}, ${city.country}`,
      latitude: city.latitude,
      longitude: city.longitude,
      score: compareTwoStrings(input, city.name),
    };

    return newCity;
  });
};

// Include distance as a factor in the score
const addDistanceScore = (data, lat, long) => {
  return data.map((city) => {
    // Get distance using the library
    let distance = geodistance.between(
      { lat, long },
      { lat: city.latitude, long: city.longitude }
    );

    let score = 1 - distance / distanceMax; // Beyond the max distance, we start taking away score
    city.score += score / 2; // equal weightings distance/string matching

    return city;
  });
};

// Sort the results, highest score first
const sortResults = (data) => {
  return data.sort((a, b) => {
    return b.score - a.score;
  });
};

export const getSuggestions = async (input, lat, long) => {
  // Keep the file in memory instead of loading it each time
  if (jsonData === undefined) {
    jsonData = await tsvToJson(tsvFile);
  }

  let results = findCities(input, jsonData);
  results = calculateScore(input, results);

  if (lat && long) results = addDistanceScore(results, lat, long);

  results = sortResults(results);

  return Promise.resolve({ suggestions: results });
};
