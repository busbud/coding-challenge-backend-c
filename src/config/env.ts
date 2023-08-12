export default {
  serverUrl: process.env.SERVER_URL || '127.0.0.1',
  serverPort: parseInt(process.env.SERVER_PORT ?? '') || 2345,
  cities: {
    acceptedCountryCodes: process.env.ACCEPTED_COUNTRY_CODES.split(','),
    largeCitiesMinimumPopulation:
      parseInt(process.env.LARGE_CITIES_POPULATION ?? '') || 5000,
  },
};
