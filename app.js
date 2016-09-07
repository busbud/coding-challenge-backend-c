var app = require('express')();
var Query = require('./repos/Query');
var City = require('./repos/City');
var Result = require('./repos/Result');
var port = process.env.PORT || 2345;

app.get('/suggestions', function (req, res) {
  if (typeof req.query.q === "undefined") {
    res.status(422).json({
      'error': "Missing the query parameter"
    });
  } else {
    let query = Query.make(req.query)
      , results = Result.convertFromMatches(query, City.getByQuery(query.search));

    res.status(results.length < 1 ? 404 : 200).json({
      suggestions: results
    });
  }
});

module.exports = app.listen(port, function() {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});
