import diacritics from 'diacritics';
import haversine from 'haversine';
import suggestionConfig from 'config/suggestion';
import cityModel from 'model/city';
import cache from 'service/cache';

export interface Location {
  latitude: number;
  longitude: number;
}

export default {
  async suggestCities(q: string, location?: Location) {
    const cacheKeyPrefix = suggestionConfig.suggestCities.cache.key;
    const cacheKeyObject = { config: suggestionConfig.suggestCities, q, location };
    const cacheTime = suggestionConfig.suggestCities.cache.time;

    return cache.getSetObject(cacheKeyPrefix, cacheKeyObject, cacheTime, async () => {
      // Normalize query string
      const normalizedQuery = diacritics.remove(q).toLowerCase();

      // Search cities matching query string
      const cities = await cityModel.searchCities(normalizedQuery);

      // Generate suggestions
      const suggestions = cities.map((city) => {
        // Calculate score
        const distance = location
          ? haversine(location, { latitude: city.lat, longitude: city.long })
          : 0;
        const populationScore = Math
          .min(1, city.population / suggestionConfig.suggestCities.score.population.max);
        const distanceScore = Math
          .max(0, 1 - distance / suggestionConfig.suggestCities.score.distance.max);
        const score = (populationScore * suggestionConfig.suggestCities.score.population.weight
          + distanceScore * suggestionConfig.suggestCities.score.distance.weight)
          / (suggestionConfig.suggestCities.score.population.weight
            + suggestionConfig.suggestCities.score.distance.weight);

        // Format suggestion
        return {
          score,
          name: `${city.name}, ${city.state}, ${city.country}`,
          latitude: city.lat,
          longitude: city.long,
        };
      });

      // Sort and limit
      return suggestions
        .sort((a: any, b: any) => b.score - a.score)
        .slice(0, suggestionConfig.suggestCities.max);
    });
  },
};
