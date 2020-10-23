const diacritics = require('diacritics')

const normalizeString = (str) => {
  let normalized = diacritics.remove(str).toLowerCase()
  return normalized.replace(/[-]/g, ' ')
}

module.exports = {
  normalizeString,
}