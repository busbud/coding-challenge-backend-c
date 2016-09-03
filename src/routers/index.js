import router from '../lib/router'
import suggest from '../services/cities/suggest'

const r = router()

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
      id: suggestion.id,
      name: `${suggestion.name}, ${suggestion.admin1}, ${formatCountry(suggestion.country)}`,
      latitude: suggestion.lat,
      longitude: suggestion.long,
      score: suggestion.score
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