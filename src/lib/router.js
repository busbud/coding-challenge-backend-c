const methods = [
  'HEAD',
  'OPTIONS',
  'GET',
  'PUT',
  'PATCH',
  'POST',
  'DELETE'
]

const methodHandler = (method, _routes = {}) => (path, controller) => {
  _routes[path] = [
    method.toUpperCase(),
    controller
  ]
}

const routes = (_routes) => () => (ctx, next) => {
  const [method, controller] = _routes[ctx.path] || []
  if (ctx.method === method) {
    return controller(ctx, next)
  }
}

export default () => {
  const _routes = {}

  const instance = {
    routes: routes(_routes)
  }

  methods.forEach((m) => {
    instance[m.toLowerCase()] = instance[m.toUpperCase()] = methodHandler(m, _routes)
  })

  return instance
}