const app = require('express')();
const suggestionsRoutes = require('./suggestions');

app.use('/suggestions', suggestionsRoutes); // load suggestion routes

// catch all routes
app.all("*", function(req,res) {
  res.status(404).send({msg: 'Not Found'});
});

module.exports = app;