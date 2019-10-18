const Fuse = require('fuse.js');

module.exports = { getCitiesSearcher };

// We use Fuse for the searches because it already provides a scoring system for the results without needing
// to calculate the Levenshtein distance ourselves. In addition, it also handles typos. 
function getCitiesSearcher(citiesData) {
  const fuse = new Fuse(citiesData, {
    keys: ['name'],
    includeScore: true,
    threshold: 0.3
  });

  return fuse.search.bind(fuse);
}