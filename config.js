module.exports = {
  env: (process.env.NODE_ENV || 'development'),
  server: {
    host: 'localhost',
    port: (process.env.PORT || 2345)
  },
  suggestionConfig: {
    minPopulation: 5000,
    maxPopulation: null,
    countryWhitelist: ['CA','US'],
    scorePrecision: 4,
    coordinateScoreWeight: 0.5
  }
};