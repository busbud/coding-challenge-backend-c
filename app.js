// Setup
var express = require('express');
var app = express();
var port = process.env.PORT || 2345;


// Suggestions
app.get('/suggestions', function(req, res){
  res.json({ 
  	suggestions:[]
  });
});


app.listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);