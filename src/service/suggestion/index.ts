import diacritics from 'diacritics';
import haversine from 'haversine';
import suggestionConfig from 'config/suggestion';
import cityModel from 'model/city';
import cache from 'service/cache';
import suggestionCache from 'service/suggestion/cache';

export interface Location {
  latitude: number;
  longitude: number;
}

export default {
  async suggestCities(q: string, location?: Location) {
    const cacheKey = suggestionCache.suggestCities.key(q, location);
    const cacheTime = suggestionCache.suggestCities.time;

    return cache.getSetObject(cacheKey, cacheTime, async () => {
      // Normalize the query string
      const normalizedQuery = diacritics.remove(q).toLowerCase();

      // Search for cities matching the query string
      const cities = await cityModel.searchCities(normalizedQuery);

      // Generate results
      const results = cities.map((city) => {
        // Calculate score
        const distance = location
          ? haversine(location, { latitude: city.lat, longitude: city.long })
          : 0;
        const populationScore = Math
          .min(1, city.population / suggestionConfig.score.population.max);
        const distanceScore = Math
          .max(0, 1 - distance / suggestionConfig.score.distance.max);
        const score = (populationScore * suggestionConfig.score.population.weight
          + distanceScore * suggestionConfig.score.distance.weight)
          / (suggestionConfig.score.population.weight + suggestionConfig.score.distance.weight);

        // Format result
        return {
          name: `${city.name}, ${city.state}, ${city.country}`,
          latitude: city.lat,
          longitude: city.long,
          score,
        };
      });

      // Sort and limit results
      return results
        .sort((a: any, b: any) => b.score - a.score)
        .slice(0, suggestionConfig.max);
    });
  },
};
