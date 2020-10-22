const App = require('./app')

const Services = require('./services')
const Datasource = require('./datasource')

const host = '127.0.0.1'
const port = process.env.PORT || 2345

const ds = new Datasource()
ds.initialize().then(() => {
  const app = App(ds)
  app.listen(port, host)
  console.log(`Server running at http://${host}:${port}/suggestions`)
})
