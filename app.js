const port = process.env.PORT || 2345;
const express = require('express');
const cors = require('cors');
const suggestionsController = require('./src/suggestionsController');


const app = express();

app.use(express.json());

// To allow cross-origin requests
app.use(cors());

app.get('/suggestions', suggestionsController.findCities);
app.listen(port, () => console.log(`Server running at http://127.0.0.1:${port}/suggestions`));

module.exports = app;
