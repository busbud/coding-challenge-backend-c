// node imports
const path = require('path');
const express = require('express');
const bcrypt = require('bcrypt');

// own imports
const security = require('./src/security');
const location = require('./src/location');

// global users array
const userPath = "./data/users.txt";
let users = security.readArray('./data/users.txt');
const ipPath = "./data/ips.txt";
let ips = security.readArray('./data/ips.txt');

// instantiate express app
const app = express();
const port = process.env.port || 3000;

// this function serves static files from the ./client directory
app.use(express.static(path.join(__dirname, 'client')));
// this enables express to accept json formatted requests
app.use(express.json());
// to parse ip address via proxy
app.set('trust proxy', true);

// register a new user
app.post("/register", async (req, res) => {
  // create a different salt for each user
  const salt = await bcrypt.genSalt(10);
  // hash the password
  const hashedPassword = await bcrypt.hash(req.body.password, salt);
  const user = {
    username: req.body.username,
    password: hashedPassword
  };
  users.push(user);
  security.appendObject(user, userPath);
  res.status(200).json({
    message: "New user created."
  });
});

// login for registered users
app.post("/login", (req, res) => {

});

// delte user account
// but first trigger middleware function to ensure that user is already authenticated
app.post("/deregister", security.validateUser, (req,res) => {

});



// handle GET requests to /suggestions
// querystrings of the form ?q=a&lat=91.0&long=18.2 are available in req.query as key:value object
app.get("/suggestions", security.validateUser, (req, res) => {
  const params = req.query;
  let query = "";
  let lat = null;
  let long = null;

  // validate q parameter
  if (/^[a-zA-Z ]+$/igm.test(params.q)){
    query = params.q;
  } else {
    res.status(400).send("Bad request.");
  }

  // validate latitude parameter
  if (params.hasOwnProperty('latitude') && parseFloat(params.latitude) <= 90.0 && parseFloat(params.latitude) >= -90.0){
    lat = parseFloat(params.latitude);
  } else {
    lat = null;
  }

  // validate longitude parameter
  if (params.hasOwnProperty('longitude') && parseFloat(params.longitude) <= 90.0 && parseFloat(params.longitude) >= -90.0){
    long = parseFloat(params.longitude);
  } else {
    long = null;
  }

  // search nearby cities
  let nearbyCities = location.search(query, lat, long);

  res.status(200).json({
    "suggestions": nearbyCities
  });
});

// express app listens on specified port
app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`)
});