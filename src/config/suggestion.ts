export default {
  suggestCities: {
    max: 5,
    score: {
      population: {
        max: 30000000,
        weight: 3,
      },
      distance: {
        max: 20000,
        weight: 1,
      },
    },
    cache: {
      key: 'suggestCities',
      time: 300,
    },
  },
};
