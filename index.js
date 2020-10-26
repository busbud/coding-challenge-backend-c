const App = require('./app')

const Datasource = require('./services/datasource')

const port = process.env.PORT || 2345

const ds = new Datasource()
ds.initialize().then(() => {
  const app = App(ds)
  app.listen(port)
  console.log(`Server running at http://127.0.0.1:${port}/suggestions`)
})
