const http = require('http')
const url = require('url')
const { RateLimiterMemory } = require('rate-limiter-flexible')

const suggestion = require('./suggestion')

const port = process.env.PORT || 2345
const limiterOptions = { points: 6, duration: 1, }

const rateLimiter = new RateLimiterMemory(limiterOptions)

module.exports = http.createServer(function (req, res) {
  rateLimiter.consume(req.ip, 2)
    .then((rateLimiterRes) => {
      if (req.url.indexOf('/suggestions') === 0) {
        try {
          const query = url.parse(req.url, true).query
          const results = { suggestions: suggestion.generate(query) }
          const statusCode = results.suggestions.length === 0 ? 404 : 200
          const headers = {
            'Content-Type': 'application/json; charset=utf-8',
            "Retry-After": rateLimiterRes.msBeforeNext / 1000,
            "X-RateLimit-Limit": limiterOptions.points,
            "X-RateLimit-Remaining": rateLimiterRes.remainingPoints,
            "X-RateLimit-Reset": new Date(Date.now() + rateLimiterRes.msBeforeNext)
          }
          res.writeHead(statusCode, headers)
          res.end(JSON.stringify(results))
        } catch (e) {
          console.log(e)
          const headers = { 'Content-Type': 'text/html' }
          res.writeHead(500, headers)
          res.end('Internal Server Error')
        }

      } else {
        res.statusCode = 400
        res.end()
      }
    })
    .catch((rateLimiterRes) => {
      const statusCode = 429
      const headers = {
        'Content-Type': 'text/html',
        "Retry-After": rateLimiterRes.msBeforeNext / 1000,
        "X-RateLimit-Limit": limiterOptions.points,
        "X-RateLimit-Remaining": rateLimiterRes.remainingPoints,
        "X-RateLimit-Reset": new Date(Date.now() + rateLimiterRes.msBeforeNext)
      }
      res.writeHead(statusCode, headers)
      res.end('Limit Exceeded')
    })
}).listen(port, '127.0.0.1')

console.log('Server running at http://127.0.0.1:%d/suggestions', port)
