const express = require('express');
const routes = require('../api/index');

const app = express();

app.use(express.json());
app.use(routes);

module.exports = app;
