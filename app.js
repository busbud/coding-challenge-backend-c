const express = require('express');

const routes = require('./app/controllers');

const app = express();
const {
    PORT = 2345,
} = process.env;

app.listen(PORT, () => {
    console.log(`Server running at http://127.0.0.1:${PORT}/suggestions`);
});
app.get('/', (req, res, next) => res.json('ok'));
app.use(routes);

module.exports = app;
