
const { getData } = require('../lib/loadData.js');

function suggestor(query, latitude, longitude){
  console.log('query: ',query);
  console.log('latitude: ',latitude);
  console.log('longitude: ',longitude);
  return getData().slice(1,10);
};

module.exports = {
  getSuggestions: suggestor
};
