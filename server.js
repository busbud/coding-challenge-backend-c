const path = require('path');
const fs = require('fs');
const express = require('express');


// instantiate express app
const app = express();
const port = process.env.port || 3000;

// this function serves static files from the ./client directory
app.use(express.static(path.join(__dirname, 'client')));

// handle GET requests to /suggestions
// querystrings of the form ?q=a&lat=91.0&long=18.2 are available in req.query as key:value object
app.get("/suggestions", (req, res) => {
  res.send(JSON.stringify(req.query));
});

// express app listens on specified port
app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`)
})