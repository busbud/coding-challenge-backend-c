var http = require('http');
var port = process.env.PORT || 6555;

var express = require('express');
var app = express();

const { makeSuggestionsQuery } = require('./domain/suggestion-query-model');
const { getHttpCode, handleError } = require('./error-handling');

const service = require('./domain/suggestion-service');

app.get('/suggestions', async (req, res) => {
  
  try {
    res.json({suggestions: await service.getSuggestions(makeSuggestionsQuery(req.query.q, req.query.latitude, req.query.longitude)) });
  } catch (err) {
    console.log({err});
    
    // return res.status(getHttpCode(err.name)).json({ message: err.message });
    return handleError(err, res);
  }
});

app.listen(port, () => {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});