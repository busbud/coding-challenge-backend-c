class City {
  isValid() {
    if (!this.name || (typeof this.name === 'string' && this.name.trim() === '')) return false
    return true
  }

  getDisplayName() {
    return `${this.name}, ${this.state}, ${this.countryName}`
  }

  getAdmin2Key() {
    return `${this.countryCode}.${this.admin1}.${this.admin2}`
  }
}

module.exports = City