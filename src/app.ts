import fastify from 'fastify';
import { City, loadCities } from './data';
import { getDistance } from 'geolib';
import { compareTwoStrings } from 'string-similarity';

export interface Suggestion {
  name: string;
  longitude: number;
  latitude: number;
  score: number;
}

interface Match {
  city: City,
  matches?: number;
  exact?: boolean;
  distance?: number;
  similarity?: number;
}

const PORT = process.env.PORT || 2345;
export const app = fastify();

/**
 * Valid query params for GET /suggestions route
 */
interface QueryParams {
  q: string;
  latitude: string;
  longitude: string;
}

function isCoordValid(lat: number, lng: number): boolean {
  return isFinite(lat) && isFinite(lng) && Math.abs(lat) <= 90 && Math.abs(lng) <= 180
}

function getSuggestionName(city: City): string {
  return `${city.name}, ${city.country}`
}

app.get('/suggestions', async (req, res) => {
  const cities = await loadCities();

  const { q, latitude, longitude } = req.query as QueryParams;

  if (!q) {
    res.status(400).send({ error: 'No query was passed' });
    return;
  }

  const [lat, lng] = [latitude, longitude].map(it => parseFloat(it));
  const hasCoords = isCoordValid(lat, lng);

  const queryRegex = new RegExp(q.replace(/\s+/g, '|'), 'gi');

  const matches: Match[] = cities
    .filter((city: City) => queryRegex.test(city.name))
    .map((city: City) => ({
      city,
      similarity: compareTwoStrings(q, city.name),
    } as Match))
    .filter(m => m.similarity > 0);

  if (matches.length === 0) {
    res.status(404).send({ suggestions: [] });
    return;
  }

  let suggestions: Suggestion[];

  if (hasCoords) {
    let minDist = 0, maxDist = 0;
    matches.forEach(m => {
      m.distance = getDistance({ lat: m.city.latitude, lng: m.city.longitude }, { lat, lng })
      minDist = Math.min(minDist, m.distance);
      maxDist = Math.max(maxDist, m.distance);
    });

    const distSpread = maxDist - minDist;
    suggestions = matches.map(m => {
      return {
        longitude: m.city.longitude,
        latitude: m.city.latitude,
        name: getSuggestionName(m.city),
        score: m.similarity * (1 - (m.distance / distSpread)), // bias score by distance
        distance: m.distance,
      }
    })
  } else {
    suggestions = matches.map(m => {
      return {
        longitude: m.city.longitude,
        latitude: m.city.latitude,
        name: getSuggestionName(m.city),
        score: m.similarity,
      }
    })
  }

  // sort by score
  suggestions = suggestions.sort((a, b) => b.score - a.score);

  res.send({ suggestions })
});

app.listen(PORT, '127.0.0.1', (err) => {
  if (!!err) {
    console.error(`Failed to start web server on port:`, err);
  } else {
    console.log(`Server is running at http://127.0.0.1:${PORT}`);
  }
})
