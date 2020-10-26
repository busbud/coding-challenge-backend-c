const diacritics = require('diacritics')
const { assert } = require('chai')

const normalizeString = (str) => {
  let normalized = diacritics.remove(str).toLowerCase()
  return normalized.replace(/[-]/g, ' ')
}

const sort = (collection, key, direction) => {
  assert(['asc', 'desc'].includes(direction), 'direction should be one of "asc" or "desc"')
  const dir = direction === 'asc' ? 1 : -1
  return collection.sort((a, b) => {
    if (a[key] === b[key]) return 0
    return a[key] > b[key] ? dir : -dir
  })

}

module.exports = {
  normalizeString,
  sort,
}