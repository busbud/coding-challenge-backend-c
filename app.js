const express = require('express');
const RateLimit = require('express-rate-limit');
const handleRequest = require('./lib/handleRequest');
const config = require('config');
const { loadData } = require('./lib/dataParser');
const { handleSuggestions } = require('./controllers/suggestionsController');
const app = express();

app.use(express.json());

const limiter = new RateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // limit each IP to 100 requests per windowMs
  delayMs: 0,
});

// apply rate-limiting to all requests
app.use(limiter);

// load cities on startup. on error, fail hard.
loadData(config.datafile, handleRequest.trie, config).catch(err => {
  console.error('err');
  console.error(err);
  process.exit();
});

app.get('/', (req, res) => {
  res.send(
    'Welcome! This site only has one API endpoint implemented. See /suggestions/?q=Londo',
  );
});

app.get('/suggestions', handleSuggestions);

module.exports = app;
