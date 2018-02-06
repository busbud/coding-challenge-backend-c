const express = require('express');
const app = express();

const port = process.env.PORT || 2345;
app.listen(port, () => console.log('App listening on port' + port));

app.get('/', (req, res) => {
  res.send('BusBud AutoSugest API!');
})

app.get('/suggestions', (req, res) => {
  res.send(JSON.stringify({
    suggestions: []
  }))
})