var fs = require('fs')
var readline = require('readline')

var colName = [
  'geonameid',
  'name',
  'asciiname',
  'alternatenames',
  'latitude',
  'longitude',
  'feature_class',
  'feature_code',
  'country_code',
  'cc2',
  'admin1_code',
  'admin2_code',
  'admin3_code',
  'admin4_code',
  'population',
  'elevation',
  'dem',
  'timezone',
  'modification_date',
]

function loadCities(filename) {

  return new Promise((res) => {
    
    var cities = []

    var rl = readline.createInterface({
      input: fs.createReadStream(filename),
    });

    rl.on('line', (line) => {
      var data = line.split('\t')
      var city = {}
      for(var idx in colName) {
        city[colName[idx]] = data[idx]
      }
      cities.push(city)
    });

    rl.on('close', () => {
      res(cities)
    })

  })

}

exports.load = async () => {

  const countries = ['CA', 'US']

  return (await loadCities('cities5000.txt')).filter((city) => {
    return countries.includes(city.country_code)
  })

}

