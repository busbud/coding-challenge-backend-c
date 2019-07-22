// General Requirements
const app               = require('express')();
// Application Code
const suggestionsRoutes = require('./suggestions');

// load suggestion routes
app.use('/suggestions', suggestionsRoutes);

// catch all routes
app.all("*", function(req,res) {
  res.status(404).send({msg: 'Not Found'});
});

module.exports = app;