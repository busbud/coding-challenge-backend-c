const express = require('express');
const app = express();
const port = process.env.PORT || 3101;
const Suggestions = require('./Services/Suggestions/router');

app.use('/suggestions', Suggestions);

app.listen(port, () => {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});
