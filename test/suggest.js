let expect  = require('chai').expect
let suggest = require('../suggest')

describe('suggestions algorithm', function() {
  let cities = [
    {
      name: 'Montréal',
      asciiname: 'Montreal'
    }
  ]
  describe('only ascii char in search', () => {
    it('works', () => {
      let suggestions = suggest(cities, 'Montreal')
      expect(suggestions[0]).to.include({
        name: 'Montréal'
      })
    })
  })
  describe('non ascii char in search', () => {
    it('normalizes special char', () => {
      let suggestions = suggest(cities, 'Montrêal')
      expect(suggestions[0]).to.include({
        name: 'Montréal'
      })
    })
  })
  describe('lowercase and uppercase', () => {
    it('is case insensitive', () => {
      let suggestions = suggest(cities, 'mONt')
      expect(suggestions[0]).to.include({
        name: 'Montréal'
      })
    })
  })
  describe('two matches', () => {
    let suggestions = []
    before(() => {
      let cities = [
        {
          name: 'Montréal',
          asciiname: 'Montreal'
        },
        {
          name: 'London',
          asciiname: 'London'
        },
        {
          name: 'Mont',
          asciiname: 'Mont'
        }
      ]
      suggestions = suggest(cities, 'Mon')
    })
    it('returns the two', () => {
      expect(suggestions.length).to.eql(2)
    })
    it('sorts by descending score', () => {
      expect(suggestions[0].score).to.be.above(suggestions[1].score)
    })
    describe('the more chars match more the score is high', () => {
      it("works", () => {
        montCity = suggestions.find(city => city.name === 'Mont')
        montrealCity = suggestions.find(city => city.name === 'Montréal')
        expect(montCity.score).to.be.above(montrealCity.score)
      })
    })
  })
  describe('search is not the begining of a name', () => {
    it('retruns an empty array', () => {
      let cities = [{
        name: 'London',
        asciiname: 'London'
      }]
      let suggestions = suggest(cities, 'ondon')
      expect(suggestions).to.be.empty
    })
  })
  describe('latitude and longitude are filled', () => {
    it('influents the results', () => {
      let cities = [{
        name: 'London, OH, USA',
        asciiname: 'London',
        latitude: 39.88645,
        longitude: -83.44825
      }, {
        name: 'London, KY, USA',
        asciiname: 'London',
        latitude: 37.12898,
        longitude: -84.08326
      },
      { // we are near hear
        name: 'London, ON, Canada',
        asciiname: 'London',
        latitude: 42.98339,
        longitude: -81.23304
      }, {
        name: 'Londontowne, MD, USA',
        asciiname: 'Londontown',
        latitude: 38.93345,
        longitude: -76.54941
      },]
      let suggestions = suggest(cities, 'Londo', 43.70011, -79.4163)
      expect(suggestions[0].name).to.eql('London, ON, Canada')
      expect(suggestions[0].score).to.be.above(suggestions[1].score)
    })
  })
})