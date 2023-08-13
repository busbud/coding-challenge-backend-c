export default {
  server: {
    url: process.env.SERVER_URL || '127.0.0.1',
    port: parseInt(process.env.SERVER_PORT ?? '') || 2345,
  },
  cache: {
    url: process.env.CACHE_URL || '',
  },
  cities: {
    acceptedCountryCodes: (process.env.ACCEPTED_COUNTRY_CODES ?? '').split(','),
    distanceScorePercentage:
      parseFloat(process.env.DISTANCE_SCORE_PERCENTAGE ?? '') || 0.5,
    largeCitiesMinimumPopulation:
      parseInt(process.env.LARGE_CITIES_POPULATION ?? '') || 5000,
    maxDistanceAddScore:
      parseInt(process.env.MAX_DISTANCE_ADD_SCORE ?? '') || 1000,
  },
};
