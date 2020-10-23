const express = require('express')
const Services = require('./services')

module.exports = ds => {
  const app = express()

  app.get('/suggestions', function (req, res) {
    if (!req.query.q) {
      res.status(400).json({ error: 'Parameter q is required.' })
      return
    }

    const services = new Services(ds)
    const suggestions = services.getSuggestions(req.query.q, req.query.latitude, req.query.longitude)
    res.status(suggestions.length > 0 ? 200 : 404).json({ suggestions })
  })

  return app
}
