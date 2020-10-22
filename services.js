class Services {
  constructor(ds) {
    this.ds = ds
  }

  getSuggestions(search, latitude, longitude) {
    const regExp = new RegExp(`^${search}`, 'i')
    const matches = this.ds.getCities().filter(x => regExp.test(x.asciiName))
    const results = matches.map(x => {
      return {
        name: x.getDisplayName(),
        latitude: x.latitude,
        longitude: x.longitude,
        score: 0,
      }
    })
    return results
  }
}

module.exports = Services