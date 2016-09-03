import 'babel-polyfill'
import app from './app'
import conf from './config'

const run = (config, fn) => {
  return app(config).listen(fn)
}

export default run

if (!module.parent) {
  run({
    port: conf.get('APP_PORT'),
    host: conf.get('APP_HOST'),
    store: conf.get('APP_STORE')
  }, (s) => {
    const {address, port} = s.address()
    console.log('Server running at http://%s:%s', address, port)
  })
}
