import { Connection } from 'api/db';
import { Request, Response } from 'express';
import { City, CityResult } from 'api/schema';

export class SuggestionController {
  private connection: Connection;

  constructor(connection: Connection) {
    this.connection = connection;
  }

  public get = async (req: Request, res: Response) => {
    try {
      const query = req.query.q;
      const lat = parseFloat(req.query.latitude?.toString() || '')
      const long = parseFloat(req.query.longitude?.toString() || '');
      if (!query) 
        return res.status(400).send({ error: "A query must be provided." });

      const raw = await this.connection.zrange(`city-name-index:${query}`, 0, -1, "WITHSCORES") 
      // If no results available, return empty set. 
      if (raw.length === 0)
        return res.status(404).send({ suggestions: [] });

      const scores = raw.filter((_element, index) => index % 2); 
      const hashes = raw.filter((_element, index) => !(index % 2));


      const suggestions = await this.connection.hmget('city-hash', hashes);

      // Parse as JSON objects and respond.
      const results = [];
      for (const [index, suggestion] of suggestions.entries()) {
        if (suggestion) {
          const city: City = JSON.parse(suggestion);
          const cityResult: CityResult =
            {
             name: city.name + ', ' + (city.territory ? city.territory + ', ' : '') + city.country,
             latitude: city.latitude,
             longitude: city.longitude, 
             score: parseFloat(scores[index])
            };

          if (lat && long) {
            const dist = distance(lat, long, city.latitude, city.longitude);
            if (dist <= 10)
              cityResult.score = Math.min(add(cityResult.score, 0.1, 10), 1.0);
            else
              cityResult.score = Math.max(add(cityResult.score, -0.3, 10), 0.0);
            
          }
          results.push(cityResult);
        }
      }

      // Sort by descending score
      results.sort((a, b) => b.score - a.score);

      return res.status(200).send({ suggestions: results });
    }
    catch (error) {
      console.error(error);
      return res.status(500).send({ error });
    }
  }
}

// Adds an offset to floating point value without changing the precision.
function add(x: number, y: number, precision: number): number {
  return (x * precision + y * precision) / precision;
}

// Calculates the distance between two lat/long coordinates (uses the Harvasine formula).
function distance(lat1: number, long1: number, lat2: number, long2: number): number {
  const radius = 6371;
  const latDiff = toRadians(lat2 - lat1);
  const longDiff = toRadians(long2 - long1);

  const a = Math.pow(Math.sin(latDiff / 2), 2) + Math.cos(toRadians(lat1)) * Math.cos(toRadians(lat2)) * Math.pow(Math.sin(longDiff / 2), 2);
  const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
  return radius * c;
}

// Converts degrees to radians.
function toRadians(degrees: number): number {
  return degrees * (Math.PI / 180);
}
