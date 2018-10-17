
var express = require("express");
var router = express.Router();
const fs = require('fs');

// app.get('/suggestions', function(req, res) {

//   var q = req.query.q
//   var latitude = req.param('latitude');
//   var longitude = req.param('longitude');

//   res.send(q + ' ' + latitude + ' ' + longitude);
// });

function parseQuery(queryString) {
    var query = {};
    var pairs = (queryString[0] === '?' ? queryString.substr(1) : queryString).split('&');
    for (var i = 0; i < pairs.length; i++) {
        var pair = pairs[i].split('=');
        query[decodeURIComponent(pair[0])] = decodeURIComponent(pair[1] || '');
    }
    return query;
}


router.get("/", function(req, res) {
  console.log("testing get suggestions end point");
});



module.exports = router;