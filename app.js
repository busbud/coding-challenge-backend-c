'use strict';

const Koa = require('koa');
const session = require('koa-session2');
const { RateLimiterMemory } = require('rate-limiter-flexible');

const { isQueryCached, cacheResults } = require('./lib/requests-cache');
const buildSuggestions = require('./lib/suggestions-builder');
const { LONGEST_CITY_NAME } = require('./lib/data-loader');

// we will allow up to 4 request per second
const rateLimiter = new RateLimiterMemory({
  points: 4,
  duration: 1,
  blockDuration: 2
});

const { PORT = '1234' } = process.env;

const app = new Koa();
app.use(session()).use(
  /**
   * @param {import('koa').Context} ctx
   */
  async ctx => {
    // Validating input
    ctx.assert(
      ctx.path === '/suggestions',
      404,
      'We have only `suggestions` endpoint here'
    );
    ctx.assert(ctx.method === 'GET', 405, 'Method not allowed');
    ctx.assert(
      ctx.query.q,
      400,
      `Bad request, expecting non-empty "q" parameter`
    );
    if (ctx.query.q.length > LONGEST_CITY_NAME && !/[,\s]/.test(ctx.query.q)) {
      // By design it should return empty array as JSON body even on 4xx errors
      ctx.body = { suggestions: [] };
      ctx.status = 404;
      return;
    }
    // if latitude or longitude is supplied they must be both valid and present both
    const [latitude, longitude] = ['latitude', 'longitude'].map(p =>
      parseFloat(ctx.query[p])
    );
    if (latitude || longitude) {
      // Longitude must be from -180 to 180
      // Latitude must be from -90 to 90
      ctx.assert(
        latitude >= -90 &&
          latitude <= 90 &&
          longitude >= -180 &&
          longitude <= 180,
        400,
        'Longitude and latitude must either both present and valid or both absent from query'
      );
    }

    // Rate limiting
    try {
      const rateLimiterRes = await rateLimiter.consume(ctx.request.ip, 1);
      ctx.set('X-RateLimit-Remaining', rateLimiterRes.remainingPoints);
    } catch (rateLimiterRes) {
      ctx.set('X-RateLimit-Remaining', rateLimiterRes.remainingPoints);
      ctx.set('Retry-After', rateLimiterRes.msBeforeNext / 1000);
      ctx.set('X-RateLimit-Limit', 3);
      ctx.set(
        'X-RateLimit-Reset',
        new Date(Date.now() + rateLimiterRes.msBeforeNext)
      );
      ctx.throw(429, 'Too many requests');
    }

    // Request cache test
    if (await isQueryCached(ctx)) return;

    // all good, let's do data
    ctx.body = {
      suggestions: buildSuggestions(ctx.query.q, latitude, longitude)
    };

    await cacheResults(ctx);
  }
);

app.once('listen', () => console.log('Server listening on port %s', PORT));

module.exports = app.listen(parseInt(PORT, 10));
