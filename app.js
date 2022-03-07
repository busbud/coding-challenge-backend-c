var http = require('http');
const { default: mongoose } = require('mongoose');
const { suggestions } = require('./src/controllers/location.controller');
require('dotenv').config();

var port = process.env.PORT || 2345;
var mongo_uri = process.env.MONGO_URI;
module.exports = http
  .createServer(async function (req, res) {
    await mongoose
      .connect(mongo_uri, {
        keepAlive: 1,
      })
      .then(() => console.log('mongoDB connected...'));

    if (req.url.indexOf('/suggestions') === 0) {
      const result = await suggestions(req, res);
      if (result.length == 0) {
        res.writeHead(404, { 'Content-Type': 'application/json' });
      } else {
        res.writeHead(200, { 'Content-Type': 'application/json' });
      }
      res.end(
        JSON.stringify({
          suggestions: result,
        })
      );
    } else {
      res.end();
    }
  })
  .listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
