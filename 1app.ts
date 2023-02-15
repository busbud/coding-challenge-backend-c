import express, { Request, Response } from 'express';
 
const app = express();

import { cities } from './cities.json';

// const { readFileSync } = require('fs');
// const tsvFileData = readFileSync('./data/cities_canada-usa.tsv', 'utf8');
// const cities = tsvJSON(tsvFileData.toString());

interface Suggestion {
  name: string;
  latitude: number;
  longitude: number;
  score: number;
}

app.get('/suggestions', (req: Request, res: Response) => {
  const { q, latitude, longitude } = req.query;
  const suggestions: Suggestion[] = [];

  // Loop through the cities to find matches for the given search term
  for (let i = 0; i < cities.length; i++) {
    const city = cities[i];
    const population = parseInt(city.population.replace(/,/g, ''));

    // Filter cities with a population above 5000 people in the USA and Canada
    if (population >= 5000 && (city.country === 'US' || city.country === 'CA')) {
      const cityName = city.name;
      const state = city.admin1;
      const country = city.country;

      // Calculate score based on search term and optionally caller's location
      let score = 0;
      if (cityName.toLowerCase().startsWith(q.toLowerCase())) {
        score = 0.8;
      } else if (cityName.toLowerCase().includes(q.toLowerCase())) {
        score = 0.6;
      }
      if (latitude && longitude) {
        const distance = getDistanceFromLatLonInKm(parseFloat(latitude), parseFloat(longitude), city.lat, city.long);
        const distanceScore = 1 / (distance + 1);
        score = score + (distanceScore * 0.2);
      }

      // Create suggestion object
      const suggestion: Suggestion = {
        name: `${cityName}, ${state}, ${country}`,
        latitude: city.lat,
        longitude: city.long,
        score,
      };
      suggestions.push(suggestion);
    }
  }

  // Sort suggestions by descending score
  suggestions.sort((a, b) => b.score - a.score);

  res.json({ suggestions });
});

// Function to calculate distance between two points given their latitude and longitude
function getDistanceFromLatLonInKm(lat1: number, lon1: number, lat2: number, lon2: number): number {
  const R = 6371; // Radius of the earth in km
  const dLat = deg2rad(lat2 - lat1);
  const dLon = deg2rad(lon2 - lon1);
  const a =
    Math.sin(dLat / 2) * Math.sin(dLat / 2) +
    Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) *
    Math.sin(dLon / 2) * Math.sin(dLon / 2);
  const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
  const d = R * c; // Distance in km
  return d;
}

// Function to convert degrees to radians
function deg2rad(deg: number): number {
  return deg * (Math.PI / 180);
}


function tsvJSON(tsv) {
  const lines = tsv.split("\n");
  const result = [];
  const headers = lines[0].split("\t");

  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const currentline = lines[i].split("\t");

    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = currentline[j];
    }

    result.push(obj);
  }

  return result;
}


// Start the server
const port = 3000;
app.listen(port, () => {
  console.log(`Server started on port ${port}`);
});
