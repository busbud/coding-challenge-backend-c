import Koa from 'koa'
import routers from './routers'
import error from './lib/error'

const listening = (fn) => {
  const noop = () => {
  }

  if (!fn) {
    fn = noop
  }

  return function() {
    fn(this)
  }
}

export default (config = {}) => {
  const app = {}
  const httpServer = app.httpServer = new Koa()

  httpServer.use(error())
  httpServer.use(routers.routes())

  app.listen = (fn) => {
    return httpServer.listen(
      config.port,
      listening(fn)
    )
  }

  return app
}
