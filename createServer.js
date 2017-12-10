const http = require('http')
const url = require("url")
const fs = require('fs')
/**
 * Returns true if the url of the requets match the patch
 * 
 * @param {String} path 
 * @param {IncomingMessage} req
 * @returns {Boolean}
 */
const routeIs = (path, req) =>
  req.url.indexOf(path) === 0

/**
 * Responds a text message.
 * Option statusCode is 200 by default
 * 
 * @param {String} text 
 * @param {ServerResponse} res 
 * @param {Object} [{statusCode = 200}={}]
 */
const respondText = (text, res, {statusCode = 200} = {}) => {
  res.writeHead(statusCode, { 'Content-Type': 'text/plain' })
  res.end(text)
}
/**
 * Responds a Json.
 * Option statusCode is 200 by default
 * 
 * @param {Object} json 
 * @param {ServerResponse} res 
 * @param {Object} [{statusCode = 200}={}] 
 */
const respondJSon = (json, res, {statusCode = 200} = {}) => {
  res.writeHead(statusCode, { 'Content-Type': 'application/json' })
  res.end(JSON.stringify(json))
}
/**
 * 
 * 
 * @param {IncomingMessage} req 
 * @returns {{q: String, latitude?: Number, longitude?: Number}} parameters
 */
const getSuggestionsParameters = (req) => {
  const parsedUrl = url.parse(req.url, true)
  const queryAsObject = parsedUrl.query
  let qLatitude = parseFloat(queryAsObject.latitude)
  let qLongitude = parseFloat(queryAsObject.longitude)
  return {
    q: queryAsObject.q || '',
    latitude: qLatitude > -90 && qLatitude < 90 ?
      qLatitude : undefined,
    longitude: qLongitude > -180 && qLongitude < 180 ?
      qLongitude : undefined
  }
}
/**
 * Returns the server.
 * It takes some dependencies =
 *    the cities
 *    the suggest function
 * 
 * @param {{cities: Objecy[], suggest: function}} mainDependencies
 * @param {port: Number} options
 * @returns {Server}
 */
module.exports = function createServer({cities, suggest}, {port}) {
  const httpServer = http.createServer((req, res) => {

    if (routeIs('/hello', req)) {
      respondText('Hello CI and CD :)', res)
    }

    else if (routeIs('/index.html', req)) {
      fs.createReadStream('./public/index.html').pipe(res)
    }

    else if (routeIs('/suggestions', req)) {
      const params = getSuggestionsParameters(req)
      // Validate params
      if (params.q.length < 3) {
        respondJSon({
          error: 'q parameter is required and has to have at least 3 chars'
        }, res, {statusCode: 400})
        return
      }

      const response = {
        suggestions: suggest(cities, params.q,params.latitude, params.longitude)
        // format the suggestions
        .map(city => ({
          name: [city.name, city.adminCode1, city.countryCode].join(', '),
          latitude: city.latitude,
          longitude: city.longitude,
          score: city.score
        }))
      }
      
      response.suggestions.length === 0 ?
        respondJSon(response, res, {statusCode: 404}) :
        respondJSon(response, res, {statusCode: 200})
    }
    
    else {
      respondText('Not found :/', res, {statusCode: 404})
    }
  })
  httpServer.listen(port, '0.0.0.0')
  console.log(`Listening at :${port}`)

  return httpServer
}