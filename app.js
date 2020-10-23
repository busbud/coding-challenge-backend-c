const express = require('express')
const Services = require('./services')

module.exports = ds => {
  const app = express()

  app.get('/suggestions', function (req, res) {
    // TODO: validate params

    const services = new Services(ds)
    const suggestions = services.getSuggestions(req.query.q, req.query.latitude, req.query.longitude)
    res.status(suggestions.length > 0 ? 200 : 404).json({ suggestions })
  })

  return app
}
