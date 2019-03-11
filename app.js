const express = require('express');
const bodyParser = require('body-parser');
const compression = require('compression');
const cors = require('cors');

const port = process.env.PORT || 2345;
const env = process.env.NODE_ENV = process.env.NODE_ENV || 'development';
const config = require('./data/config')[env];


// Load Database Related Functionality
require('./data/db')(config).then(() => console.log('Connected to DB'));

const CityModel = require('./data/city');
const ConstantModel = require('./data/constant');


// Import Utility functions
const transformForClient = require('./utils').transformForClient;
const transformCitiesForClient = require('./utils').transformCitiesForClient;
const generateScore = require('./utils').generateScore;
const sortByScore = require('./utils').sortByScore;
const updateMinAndMaxDist = require('./utils').updateMinAndMaxDist;

// Update the Minimum and Maximum distanced from values calculated from the cities in the dataset
updateMinAndMaxDist(ConstantModel);

// Setup Express
const app = express();
app.use(bodyParser.urlencoded({ extended: false }));
app.use(bodyParser.json());
app.use(compression());
app.use(cors());

const cacheMiddleware = require('./cache');

app.get('/suggestions', cacheMiddleware, (req, res) => {
  const suggestions = [];
  try{

    let q = '';
    let latitude = -1;
    let longitude = -1;

    // Use try catch to handle invalid request from the user
    try{
      q = req.query.q ||'';
      latitude = parseFloat(req.query.latitude || -1);
      longitude = parseFloat(req.query.longitude || -1);

    }catch (paramErr) {
      console.error(paramErr);
      res.status(400).send('Invalid parameters specified');
      return;
    }

    console.log(`q is ${q}, long is ${longitude}, lat is ${latitude}`);

    // If the
    if (q.length >= 2){
      CityModel
          .find({ name:{ $regex: new RegExp("^" + q, "ig") } }) // Filter based on the string that starts with text in q
          .exec((err, cities) => {
            let results = [];
            if (cities.length > 0) {
              results = cities.map(transformForClient)
                  .map(city => generateScore(city, q, latitude, longitude))
                  .sort(sortByScore);
            }else{
              res.status(404);
            }

            res.json({'suggestions': results });
          });

    }else{
      res.json(suggestions);
    }

  }catch (e) {
    console.error(e);
    res.status(500).send("The server encountered an error: " + e);
  }
});

app.get('/cities',cacheMiddleware, (req, res) => {
  try{

    let offset = parseInt(req.query.offset || 0);
    let limit = parseInt(req.query.limit || 10);

    // console.log(`Offset is ${offset} and Limit is ${limit}`);

    // Make the request and send the response using a callback
    CityModel
        .find({})
        .skip(offset)
        .limit(limit)
        .exec((err, cities) => res.json(cities.map(transformCitiesForClient)));
  }catch (e) {
    console.error(e);
    res.status(500).send("The server encountered an error: " + e);
  }
});

module.exports = app;
app.listen(port, () => console.log('Server running at http://127.0.0.1:%d/suggestions', port));

// var http = require('http');
// const port = process.env.PORT || 2345;
// module.exports = http.createServer(function (req, res) {
//   res.writeHead(404, {'Content-Type': 'text/plain'});
//
//   if (req.url.indexOf('/suggestions') === 0) {
//     res.end(JSON.stringify({
//       suggestions: []
//     }));
//   } else {
//     res.end();
//   }
// }).listen(port, '127.0.0.1');

// console.log('Server running at http://127.0.0.1:%d/suggestions', port);