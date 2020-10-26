const express = require('express')
const morgan = require('morgan')

const config = require('./config')
const Services = require('./services')

module.exports = ds => {
  const app = express()

  app.get('/suggestions', function (req, res) {
    if (!req.query.q) {
      return res.status(400).json({ error: 'Parameter q is required.' })
    }

    let latitude, longitude
    if (req.query.latitude || req.query.longitude) {
      latitude = Number(req.query.latitude)
      longitude = Number(req.query.longitude)
      if (isNaN(latitude) || isNaN(longitude) || Math.abs(latitude) > 90 || Math.abs(longitude) > 180) {
        return res.status(400).json({ error: 'Supplied latitude and longitude parameters are incorrect.' })
      }
    }

    const services = new Services(ds)
    const suggestions = services.getSuggestions(req.query.q, latitude, longitude)
    res.status(suggestions.length > 0 ? 200 : 404).json({ suggestions })
  })

  app.use((err, req, res, next) => {
    if (res.headersSent) {
      return next(err)
    }

    if (config.DEBUG) {
      console.error(err)
    }

    res.status(500).end()
  })

  return app
}
