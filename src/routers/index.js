import router from '../lib/router'
import suggest from '../services/cities/suggest'

const r = router()

const CA_ADMIN1 = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '08': 'ON',
  '09': 'PE',
  '10': 'QC',
  '11': 'MB',
  '12': 'YT',
  '13': 'NT',
  '14': 'NU'
}

const formatState = (code) => {
  if (CA_ADMIN1[code]) {
    return CA_ADMIN1[code]
  }
  return code
}

const formatCountry = (c) => {
  switch (c) {
    case 'CA':
      return 'CANADA'
    case 'US':
      return 'USA'
    default:
      return c
  }
}

const formatSuggestions = (suggestions) => {
  return suggestions.map((suggestion) => {
    return {
      name: `${suggestion.name}, ${formatState(suggestion.admin1)}, ${formatCountry(suggestion.country)}`,
      latitude: suggestion.lat,
      longitude: suggestion.long,
      score: parseFloat(suggestion.score).toFixed(2)
    }
  })
}

r.get('/suggestions', (ctx) => {
  const {q, latitude, longitude} = ctx.query
  return suggest(q, longitude, latitude)
    .then(formatSuggestions)
    .then((suggestions) => {
      ctx.body = {
        suggestions
      }
    })
})

export default r
