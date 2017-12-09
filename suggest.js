const normalize = (string) =>
  string
  .normalize('NFD')
  .replace(/[\u0300-\u036f]/g, "")
  .toLowerCase()

const calculateScrore = (city, normalizedSearch) =>
  1 - (city.asciiname.length - normalizedSearch.length) * 0.1

function suggest(cities, search) {
  const normalizedSearch = normalize(search)
  return cities
    .filter(city =>
      normalize(city.asciiname).indexOf(normalizedSearch) == 0
    )
    .map(city => 
      Object.assign(city, {score: calculateScrore(city, normalizedSearch)})
    )
    .sort((a,b) => b.score - a.score)
}

module.exports = suggest