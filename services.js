class Services {
  constructor(ds) {
    this.ds = ds
  }

  getSuggestions(search, latitude, longitude) {
    const regExp = new RegExp(`^${search}`, 'i')
    const matches = this.ds.cities.filter(x => regExp.test(x.name))
    return matches
  }
}

module.exports = Services