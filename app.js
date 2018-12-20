const express = require('express');
const apicache = require('apicache');

const routes = require('./routes');


const PORT = process.env.PORT || 4000;

const app = express();
const cache = apicache.middleware;

app.use(cache('5 minutes'));
app.use(routes);

app.listen(PORT, () => `Server running at http://127.0.0.1:${PORT}/suggestions'`);

module.exports = app;