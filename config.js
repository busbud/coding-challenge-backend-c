module.exports = {
  env: (process.env.NODE_ENV || 'development'),
  server: {
    host: 'localhost',
    port: (process.env.PORT || 2345)
  },
  suggestions: {
    minPopulation: 5000,
    maxPopulation: null,
    countryWhitelist: ['CA','US']
  }
};

//
// id: '5881791',
//   name: 'Abbotsford',
//   ascii: 'Abbotsford',
//   alt_name: 'Abbotsford,YXX,Абботсфорд',
//   lat: '49.05798',
//   long: '-122.25257',
//   feat_class: 'P',
//   feat_code: 'PPL',
//   country: 'CA',
//   cc2: '',
//   admin1: '02',
//   admin2: '5957659',
//   admin3: '',
//   admin4: '',
//   population: '151683',
//   elevation: '',
//   dem: '114',
//   tz: 'America/Vancouver',
//   modified_at: '2013-04-22'
//
//
//
//
// Design an API endpoint that provides auto-complete suggestions for large cities. ]
// The suggestions should be restricted to cities in the USA and Canada with a population above 5000 people.
//
