var http = require('http');
var port = process.env.PORT || 2345;

var geolib = require('geolib')

var testResolve

exports.waitForInit = () => {
  return new Promise((res) => {
    testResolve = res
  })
}

(async () => {

  var dataset = await require('./dataset').load()

  const admin1_lut = {
    '08': 'ON',
    '10': 'QC',
  }

  const country_lut = {
    'CA': 'Canada',
    'US': 'USA',
  }

  function buildName(m) {
    return `${m.asciiname}, ${admin1_lut[m.admin1_code] || m.admin1_code}, ${country_lut[m.country_code]}`
  }

  function sortByDistance(coord, matches) {
    var list = geolib.orderByDistance(coord, matches).map((order) => ({
      ...matches[order.key],
      distance: order.distance,
      score: 1,
    }))
    if(list.length > 1) {
      var near = list[0].distance
      var far = list[list.length - 1].distance
      list = list.map((m) => ({
        ...m,
        score: (1 - (m.distance - near) / (far - near)),
      }))
    }
    return list
  }

  async function suggestions(q, coord) {
    // Avoid sorting too many matches if q is less than 3 characters
    if(coord && q.length < 3) {
      return []  
    }
    var rgx = new RegExp(q, 'i')
    var matches = dataset.filter((city) => {
      return rgx.test(city.alternatenames)
    })
    if(coord) {
      return sortByDistance(coord, matches).map((m) => ({
        name: buildName(m),
        latitude: m.latitude, 
        longitude: m.longitude,
        score: m.score,
      }))
    } else {
      return matches.map((m) => ({
        name: buildName(m),
        latitude: m.latitude, 
        longitude: m.longitude,
        score: 1,
      }))
    }
  }

  var server = http.createServer(function (req, res) {
    var myurl = new URL(req.url, 'https://example.org/')
    
    if (myurl.pathname == '/suggestions') {
      var q = myurl.searchParams.get('q')
      var latitude = myurl.searchParams.get('latitude')
      var longitude = myurl.searchParams.get('longitude')
      
      if(latitude && longitude) {
        var coord = {
          latitude, 
          longitude,
        }
      }
      suggestions(q, coord)
      .then((list) => {
        res.writeHead(list.length ? 200 : 404, {'Content-Type': 'application/json'})
        res.end(JSON.stringify({ 'suggestions': list }, null, 4))
      })
      .catch((err) => {
        console.log(err)
        res.writeHead(404, {'Content-Type': 'application/json'})
        res.end(JSON.stringify({ 'suggestions': [] }, null, 4))
      })
    } else {
      res.writeHead(404, {'Content-Type': 'application/json'})
      res.end()
    }
  })
  .listen(port, '127.0.0.1')

  if(testResolve) {
    testResolve(server)
  }

})()

console.log('Server running at http://127.0.0.1:%d/suggestions', port)