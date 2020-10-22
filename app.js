var express = require('express')

const Services = require('./services')
const Datasource = require('./datasource')

const port = process.env.PORT || 2345
const app = express()


const ds = new Datasource()

ds.initialize().then(() => {

  app.get('/suggestions', function (req, res) {
    const services = new Services(ds)
    const suggestions = services.getSuggestions(req.query.q, req.query.latitude, req.query.longitude)
    res.json({ suggestions })
  })

  app.listen(port)

  console.log('Server running at http://127.0.0.1:%d/suggestions', port)
})
