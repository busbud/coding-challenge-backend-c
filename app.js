if (process.env.NODE_ENV !== 'production') {
  require('dotenv').config();
}
const express = require('express');
const pg = require('pg');
const dbConfig = require('./config/db');
const inputValidator = require('./validators/inuput-validator');
const Pool = pg.Pool

const app = express();

const pool = new Pool(dbConfig);

const port = process.env.PORT || 5000;

app.use(express.json());
app.use(express.urlencoded({extended : true}));

app.get('/', (req, res) => {
 res.status(200).json('Welcome to our auto complete API ! To use the API please target the /suggestion endpoint');
 return;
});

app.use(inputValidator);

require('./routes/suggestion-routes')(app, pool)

app.listen(port, () => {
  console.log(`Server running at http://127.0.0.1:${port}/suggestions`);
});


module.exports = app;