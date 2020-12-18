var http = require('http');
var port = process.env.PORT || 2345;

var express = require('express');
var app = express();

const { makeSuggestionsQuery } = require('./domain/suggestion-query-model');
const { getHttpCode } = require('./error-handling');

const service = require('./domain/suggestion-service');

app.get('/suggestions', async (req, res) => {
  try {
    res.json(await service.getSuggestions(makeSuggestionsQuery(req.query.q, req.query.latitude, req.query.longitude)));
  } catch (err) {
    console.log({err});
    res.status(getHttpCode(err.name)).json({ message: err.message });
  }
});

app.listen(port, () => {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});