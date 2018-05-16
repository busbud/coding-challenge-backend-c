const express = require('express');
const ev  = require('express-validator');
const port = process.env.PORT || 2345;
const cities = require('./lib/cities');

const app = express();

app.use(ev());
app.use(express.static('public'));

app.get("/", (req, res) => {
  res.sendfile('./public/index.html');
});

app.get("/suggestions", (req, res) => {

  req.check('q', "Must be alphabetic only with whitespace and dashes or dot.").notEmpty().isAscii;
  req.check('q', "Must be at least 2 chars before the autocomplete works.").isLength({min: 2});

  if(req.query.lat != undefined) {
    req.check('lat', "Must be a valid coordinate (latitude).").isFloat();
  }

  if(req.query.long != undefined) {  
    req.check('long', "Must be a valid coordinate (longitude).").isFloat();
  }

  let errors = req.validationErrors(true);
  if(errors) {
    res.status(404).json({
      errors: errors,
      suggestions: []
    });
    return;
  }

  let search = req.query.q;
  let latitude = req.query.lat ? req.query.lat : null;
  let longitude = req.query.long ? req.query.long : null;
  
  cities.search({ q: search.toLowerCase(), lat: latitude, long: longitude }, (data) => {
    if (data.length == 0) {
      res.status(404).json({
        errors: "Nothing was found",
        suggestions: []
      });
      return;
    }

    data.sort((a, b) => {
      return b.finalScore < a.finalScore ? -1 : b.finalScore > a.finalScore ? 1 : 0;
    });

    res.status(200).json({
      suggestions: data
    });
  });

});

app.listen(port, () => {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});

module.exports = app;
