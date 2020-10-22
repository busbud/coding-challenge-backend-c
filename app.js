const http = require('http')
const querystring = require('querystring')

const Services = require('./services')
const Datasource = require('./datasource')

const port = process.env.PORT || 2345

const ds = new Datasource()

ds.initialize().then(() => {

  module.exports = http.createServer(function (req, res) {
    res.writeHead(404, { 'Content-Type': 'text/plain' })

    if (req.url.indexOf('/suggestions') === 0) {
      const [, qs] = req.url.split('?')
      const params = querystring.parse(qs)

      const services = new Services(ds)
      const suggestions = services.getSuggestions(params.q, params.latitude, params.longitude)

      res.end(JSON.stringify({
        suggestions: suggestions,
      }))
    } else {
      res.end()
    }
  }).listen(port, '127.0.0.1')

  console.log('Server running at http://127.0.0.1:%d/suggestions', port)
})
