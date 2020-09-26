const {sanitize} = require('../helpers/sanitizer');
const {printDate} = require('../helpers/logger');
const City = require('../models/City');

module.exports = (res, reqUrl) => {
  const sanitized = sanitize(reqUrl.query.q.trim()).split(' ').join('|');
  const coordinates = !!reqUrl.query.longitude && !!reqUrl.query.latitude;
  City.find({name: {$regex: sanitized, $options: 'i'}, population: {$gt: 5000}})
      .cache(300)
      .then((cities) => {
        res.writeHead(cities.length > 0 ? 200 : 404, {'Content-Type': 'application/json'});
        console.log(`${printDate()}  ${reqUrl.href}  ${cities.length > 0 ? cities.length : 'No'} cities found`);
        const parsedCities = cities.map((city) => City.parse(city, reqUrl.query, coordinates));
        res.end(JSON.stringify({
          suggestions: parsedCities.sort((a, b) => (a.score < b.score) ? 1 : ((b.score < a.score) ? -1 : 0)),
        }));
      }).catch((error) => {
        console.error(`${printDate()}  ${reqUrl.href}  An error occurred while fetching cities: ${error}`);
        res.writeHead(500, {'Content-Type': 'application/json'});
        res.end(JSON.stringify({status: 500, message: 'Internal Server Error'}));
      });
};
