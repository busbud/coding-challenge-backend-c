const http = require('http');
const port = process.env.PORT || 2345;

const express = require('express');

const app = express();
const fs = require('fs');
const bodyParser = require('body-parser');
const url = require('url');
const querystring = require('querystring');
// var suggestionsRouter = require('./api/suggestions');

// app.use('/api/suggestions', suggestionsRouter);
app.get('/suggestions', function(req, res) {
    if (req.url.indexOf('/suggestions') === 0) {
    res.status(404);
      }
    res.setHeader('Content-Type', 'application/json')
    // res.json({ name: req.query.q,
    //   latitude: req.query.latitude,
    //   longitude: req.query.longitude })
     fs.readFile('./data/cities_canada-us.json', function(err, data){

      let arr = JSON.parse(data);
      arr.forEach(function(city) {
        console.log(city.name);
      });


      // res.write(data.toString());
      return res.end();
    });


});
// if no match
// {
//   "suggestions": []
// }

// function parseQuery(queryString) {
//     var query = {};
//     var pairs = (queryString[0] === '?' ? queryString.substr(1) : queryString).split('&');
//     for (var i = 0; i < pairs.length; i++) {
//         var pair = pairs[i].split('=');
//         query[decodeURIComponent(pair[0])] = decodeURIComponent(pair[1] || '');
//     }
//     console.log("console in parse function", query);
//     return query;
// }

app.listen(port, (err) => {
  if (err) {
    return console.log('err', err)
  }

  console.log(`Server running at http://127.0.0.1:${port}/suggestions`);
})