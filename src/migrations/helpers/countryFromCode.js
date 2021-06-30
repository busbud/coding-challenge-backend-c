const countries = {
  US: 'USA',
  CA: 'Canada'
}

module.exports = (code) => {
  const country = countries[code]
  if (!country) {
    return code
  }

  return country
}
