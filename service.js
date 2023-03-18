import { cities } from './cities.js'

function getDistanceFromLatLonInKm(lat1,lon1,lat2,lon2) {
  const R = 6371; // Radius of the earth in km
  const dLat = deg2rad(lat2-lat1);  // deg2rad below
  const dLon = deg2rad(lon2-lon1); 
  const a = 
    Math.sin(dLat/2) * Math.sin(dLat/2) +
    Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) * 
    Math.sin(dLon/2) * Math.sin(dLon/2)
    ; 
  const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a)); 
  const d = R * c; // Distance in km
  return d;
}

function deg2rad(deg) {
  return deg * (Math.PI/180)
}
export class Service {
  constructor() {}

  async getSuggestions(req, resp) {
    const partial_name = req.query.q
    const partial_matcher = new RegExp(`${partial_name}`, 'i')
    const s = cities.c.filter(city => {
      if (city != undefined) {
        return partial_matcher.test(city.name)
      } else {
        return false
      }
    }).map(city => {
      const { name, lat, country, admin1 } = city
      const lon = city['long']
      const subRegionMap = (country, admin1) => {
        const rmap = {
          '01': 'AB',
          '02': 'BC',
          '03': 'MB',
          '04': 'NB',
          '05': 'NL',
          '07': 'NS',
          '08': 'ON',
          '09': 'PE',
          '10': 'QC',
          '11': 'SK',
          '12': 'YT',
          '13': 'NT',
          '14': 'NV'
        }
        if (country == 'CA') {
          return rmap[admin1]
        } else {
          return admin1
        }
      }

      let score = "0.0"

      if (!req.query.latitude || !req.query.longitude) {
        // give a score between 0 and 1 based on amount of matching text in the name
        score = `${(partial_name.length / name.length).toFixed(1)}`
      }

      return {
        name: `${name}, ${subRegionMap(country, admin1)}, ${country}`,
        latitude: lat,
        longitude: lon,
        score
      }
    })

    for(const city in s) {
      if (req.query.latitude && req.query.longitude) {
        const distance = getDistanceFromLatLonInKm(city.lat, city['long'], req.query.latitude, req.query.longitude)
        s.distance = distance
      }
    }
    

    // we can be pretty confident if there is only one result
    // e.g. "Thunder Bay"
    if (s.length === 1) {
      s[0].score = "0.9"
    }
    return {
      suggestions: s.sort((a,b) => {
        return a.distance > b.distance
      })
    }
  }
}
