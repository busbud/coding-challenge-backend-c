// node imports
const path = require('path');
const express = require('express');
const cors = require('cors');
const jwt = require('jsonwebtoken');
const bcrypt = require('bcrypt');

// own imports
const security = require('./src/security');
const location = require('./src/location');


// global variables
const jwtKey = "tHiSiSaVeRySeCrEtKeY";
const jwtExpirySeconds = 300;
// temporary file paths
const userPath = "./data/users.txt";
const ipPath = "./data/ips.txt";
// download from s3
security.downloadFile(userPath, 'users.txt');
security.downloadFile(ipPath, 'ips.txt');
// files into memory
let users = security.readArray(userPath) || [];
let ips = security.readArray(ipPath) || [];

// instantiate express app
let app = express();
const port = process.env.PORT || 3000;

// to parse ip address via proxy
app.set('trust proxy', true);
// handle cors
app.use(cors());
app.use(function(req, res, next) {
  res.header("Access-Control-Allow-Origin", "*");
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
  next();
});
// this enables express to accept json formatted requests
app.use(express.json("*/json"));
// this function serves static files from the ./client directory
app.use(express.static(path.join(__dirname, 'client')));


// register a new user
app.post("/register", async (req, res) => {
  // input validation username: allowed alphanumeric and _
  if (!/^[a-zA-Z0-9_]+$/.test(req.body.username)){
    return res.status(400).send("Usernames can only contain alphanumeric characters and underscores.");
  }
  // input validation username: allowed alphanumeric and _$*+
  if (!/^[a-zA-Z0-9_$*+!]+$/.test(req.body.password)){
    return res.status(400).send("Passwords can only contain alphanumeric characters and _$*+!");
  }

  // check if requested ip has created too many users already
  let [creationAllowed, updatedIps] = security.checkIp(req.ip, ips);
  // save updated ip list
  ips = updatedIps;
  security.writeArray(ips, ipPath);
  security.uploadFile(ipPath, "ips.txt");

  // proceed according to the creationAllowed value
  if (!creationAllowed) { // ip forbidden
    return res.status(403).send("You are temporarily blocked, because you created too many users.");
  } else { // ip allowed, proceed
    // check if username already exists
    if ( users.findIndex((el) => el.username === req.body.username) > -1) { // username already taken
      // Note: in this case users counter in ips is increased regardless, this is not quite logical and could be changed
      return res.status(409).send("This username is already taken, try a different one.");
    } else {
      // create new user object and update users list
      console.log("req.body:");
      console.log(req.body);
      let newUser = await security.encryptUserPw(req.body);
      users.push(newUser);
      security.appendObject(newUser, userPath);
      security.uploadFile(userPath, "users.txt");

      return res.status(201).send("New user created.");
    }
  }
});

// login for registered users
app.post("/login", async (req, res) => {
  // input validation username: allowed alphanumeric and _
  if (!/^[a-zA-Z0-9_]+$/.test(req.body.username)){
    return res.status(400).send("Usernames can only contain alphanumeric characters and underscores.");
  }
  // input validation username: allowed alphanumeric and _$*+
  if (!/^[a-zA-Z0-9_$*+!]+$/.test(req.body.password)){
    return res.status(400).send("Passwords can only contain alphanumeric characters and _$*+!");
  }
  // check if user exists and password is correct
  const userIndex = users.findIndex((el) => el.username == req.body.username);

  // user does not exist
  if (userIndex === -1){
    return res.status(403).send("User does not exist or wrong password.");
  }
  // user exists but wrong password 
  if (!await bcrypt.compare(req.body.password, users[userIndex].password)) {
    return res.status(403).send("User does not exist or wrong password.");
  }

  // else user successfully authenticated
  // see https://www.sohamkamani.com/blog/javascript/2019-03-29-node-jwt-authentication/
  const token = jwt.sign({ username: req.body.username }, jwtKey, {
		algorithm: "HS256",
		expiresIn: jwtExpirySeconds,
	});
	// set the cookie as the token string, with a similar max age as the token
	// here, the max age is in milliseconds, so we multiply by 1000
  res.cookie("token", token, { maxAge: jwtExpirySeconds * 1000 });
  return res.status(202).send("Access token created.");
});

// delte user account
// but first trigger middleware function to ensure that user is already authenticated
app.delete("/deregister", security.authenticateUser, (req,res) => {
  let userIndex = users.findIndex((el) => el.username === req.user.username);
  let deletedUser = users.splice(userIndex,1);
  // write to persistent file
  security.writeArray(users, userPath);
  security.uploadFile(userPath, "users.txt");
  return res.status(200).send("Deleted user " + deletedUser[0].username);
});



// handle GET requests to /suggestions
// querystrings of the form ?q=a&lat=91.0&long=18.2 are available in req.query as key:value object
app.get("/suggestions", security.authenticateUser, (req, res) => {
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
  console.log(`Example app listening at ${port}`)
});