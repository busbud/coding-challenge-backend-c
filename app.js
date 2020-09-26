// external dependencies
const http = require('http');
const url = require('url');
const mongoose = require('mongoose');
const cachegoose = require('cachegoose');
require('dotenv').config();

// internal requires and utilities
const {printDate} = require('./helpers/logger');
const suggestions = require('./routes/suggestions');

// database configuration & initialization
const db = process.env.MONGO_URI;
const databaseConfiguration = {
  useNewUrlParser: true,
  useUnifiedTopology: true,
  useCreateIndex: true,
};
cachegoose(mongoose);
mongoose
    .connect(db, databaseConfiguration)
    .then(() => console.log(`${printDate()}  Successful connection to MongoDB.`))
    .catch((error) => console.error(`${printDate()}  An error occurred while trying to connect to database: ${error}`));

const port = process.env.PORT || 2345;

module.exports = http.createServer((req, res) => {
  const reqUrl = url.parse(req.url, true);
  if (reqUrl.pathname === '/suggestions') {
    suggestions(res, reqUrl);
  } else {
    console.error(`${printDate()} ${reqUrl.href}  Error 404 Not Found`);
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end('404 Not Found');
  }
}).listen(port);

console.log(`${printDate()}  Server running at http://127.0.0.1:${port}/suggestions`);
