const Fuse = require('fuse.js');

module.exports = { getCitiesSearcher };

function getCitiesSearcher(citiesData) {
  const fuse = new Fuse(citiesData, {
    keys: ['name'],
    includeScore: true
  });

  return fuse.search.bind(fuse);
}