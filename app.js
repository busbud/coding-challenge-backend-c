/* eslint-disable no-console */
require('dotenv').config();

// Libraries
const Koa = require('koa');
const KoaBody = require('koa-body');
const Etag = require('etag');
const Uuid = require('uuid');

const home = require('./routes/home');
const suggestions = require('./routes/suggestions');


// Utilties
const ApiError = require('./utilties/ApiError');

const port = process.env.PORT || 2345;

const app = new Koa();

// middleware
app.use(KoaBody());

// CORS setting middleware
app.use(async (ctx, next) => {
  if (ctx.method === 'OPTIONS') {
    if (!ctx.get('Access-Control-Request-Method')) {
      // this not preflight request, ignore it
      await next();
    }

    ctx.set('Access-Control-Allow-Origin', '*');
    ctx.set('Access-Control-Allow-Credentials', 'true');
    ctx.set('Access-Control-Allow-Methods', 'GET');
    ctx.set('Access-Control-Allow-Headers', 'content-type');
    ctx.status = 204;
  } else {
    // Simple Cross-Origin Request, Actual Request, and Redirects
    ctx.set('Access-Control-Allow-Origin', '*');
    ctx.set('Access-Control-Allow-Credentials', 'true');
    ctx.set('Vary', 'Origin');
    await next();
  }
});

// JSON helper
app.use(async (ctx, next) => {
  ctx.json = (body, code) => {
    let httpCode = code;

    ctx.body = body || { data: [] };
    ctx.body.meta = {
      requestId: ctx.state.requestId,
      timestamp: +new Date(),
    };

    if (ctx.body && ctx.body.data) {
      const eTag = Etag(JSON.stringify(ctx.body.data));

      const requestETag = ctx.req.headers['if-none-match'];
      if (requestETag === eTag) {
        httpCode = 304;
      } else {
        // Set ETag
        ctx.set('ETag', eTag);
      }
    }

    ctx.type = 'application/json;charset=utf-8';
    ctx.status = httpCode || 200;
  };

  await next();
});

// if not application/json
app.use(async (ctx, next) => {
  const allowedMethods = ['PUT', 'PATCH', 'POST', 'DELETE'];

  if (allowedMethods.indexOf(ctx.request.method) > -1 && ctx.request.headers['content-type'] && ctx.request.headers['content-type'].toLowerCase().indexOf('application/json') === -1) {
    throw new ApiError('Content-Type must be application/json', 406, 0);
  }

  await next();
});

// Error & Logging
app.use(async (ctx, next) => {
  ctx.state.requestId = Uuid.v4();

  try {
    await next();
  } catch (err) {
    const clientError = {
      ...err,
      message: 'Service Error',
      trace: err.stack,
    };

    ctx.status = err.status || err.statusCode || 500;
    ctx.body = {
      error: clientError,
      meta: {
        requestId: ctx.state.requestId,
        now: +new Date(),
      },
    };
  }
});

app.use(home.routes());
app.use(suggestions.routes());

// Listen
const server = app.listen(Number(port), () => {
  console.log(`Service started on port ${port}`);
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});

module.exports = server;
