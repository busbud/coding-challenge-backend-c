const env = process.env.NODE_ENV || 'development'

export default () => {
  return (ctx, next) => {
    const text = (err) => {
      ctx.type = 'text/plain'
      if (env === 'development') {
        ctx.body = err.message
      } else if (err.expose) {
        ctx.body = err.message
      } else {
        throw err
      }
    }

    const json = (err) => {
      ctx.type = 'application/json'
      if (env === 'development') {
        ctx.body = {
          error: err.message
        }
      } else if (err.expose) {
        ctx.body = {
          error: err.message
        }
      } else {
        ctx.body = {
          error: ctx.status
        }
      }
    }

    const html = (err) => {
      ctx.type = 'text/html'
      if (env === 'development') {
        ctx.body = {
          error: err.message
        }
      } else if (err.expose) {
        ctx.body = {
          error: err.message
        }
      } else {
        ctx.body = {
          error: ctx.status
        }
      }
    }

    const error = (err) => {
      const accepted = ctx.accepts('html', 'text', 'json')

      ctx.status = err.status || 500
      ctx.app.emit('error', err, ctx)

      if (accepted === 'text') {
        text(err)
      }
      if (accepted === 'html') {
        html(err)
      }
      if (accepted === 'json') {
        json(err)
      }
    }

    return next()
      .then(() => {
        if (ctx.status === 404 && !ctx.body) {
          ctx.throw(404)
        }
      })
      .catch(error)
  }
}
