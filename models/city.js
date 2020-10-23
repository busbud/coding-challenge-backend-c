class City {
  isValid() {
    if (!this.name || (typeof this.name === 'string' && this.name.trim() === '')) return false
    return true
  }

  getDisplayName() {
    return `${this.name}, ${this.state}, ${this.country}`
  }
}

module.exports = City