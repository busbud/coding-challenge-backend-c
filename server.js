// node imports
const path = require('path');
const express = require('express');
const jwt = require('jsonwebtoken');
const bcrypt = require('bcrypt');

// own imports
const security = require('./src/security');
const location = require('./src/location');

// global users array
const userPath = "./data/users.txt";
const ipPath = "./data/ips.txt";
let users = security.readArray(userPath) || [];
let ips = security.readArray(ipPath) || [];
const jwtKey = "tHiSiSaVeRySeCrEtKeY";
const jwtExpirySeconds = 300;


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
  try {
    // input validation username: allowed alphanumeric and _
    if (!/^[a-zA-Z0-9_]+$/.test(req.body.username)){
      res.status(400).json({
        message: "Usernames can only contain alphanumeric characters and underscores."
      });
      return 0; // this is nessecary, so that only one result at a time is returned
    }
    // input validation username: allowed alphanumeric and _$*+
    if (!/^[a-zA-Z0-9_$*+!]+$/.test(req.body.password)){
      res.status(400).json({
        message: "Passwords can only contain alphanumeric characters and _$*+!"
      });
      return 0;
    }

    // check if requested ip has created too many users already
    let [creationAllowed, updatedIps] = security.checkIp(req.ip, ips);
    // save updated ip list
    ips = updatedIps;
    security.writeArray(ips, ipPath);

    // proceed according to the creationAllowed value
    if (!creationAllowed) { // ip forbidden
      res.status(403).json({
        message: "You are temporarily blocked, because you created too many users."
      });
      return 0;
    } else { // ip allowed, proceed
      // check if username already exists
      if ( users.findIndex((el) => el.username === req.body.username) > -1) { // username already taken
        // Note: in this case users counter in ips is increased regardless, this is not quite logical and could be changed
        res.status(409).json({
          message: "This username is already taken, try a different one."
        });
        return 0;
      } else {
        // create new user object and update users list
        let newUser = await security.encryptUserPw(req.body);
        users.push(newUser);
        security.appendObject(newUser, userPath);

        res.status(201).json({
          message: "New user created."
        });
        return 0;
      }
    }
  } catch (err) {
    console.log(err);
    res.status(500).send("Error.");
    return 0;
  }
});

// login for registered users
app.post("/login", async (req, res) => {
  // input validation username: allowed alphanumeric and _
  if (!/^[a-zA-Z0-9_]+$/.test(req.body.username)){
    res.status(400).json({
      message: "Usernames can only contain alphanumeric characters and underscores."
    });
    return 0; // this is nessecary, so that only one result at a time is returned
  }
  // input validation username: allowed alphanumeric and _$*+
  if (!/^[a-zA-Z0-9_$*+!]+$/.test(req.body.password)){
    res.status(400).send("Passwords can only contain alphanumeric characters and _$*+!");
    return 0;
  }
  // check if user exists and password is correct
  const userIndex = users.findIndex((el) => el.username == req.body.username);

  // user does not exist
  if (userIndex === -1){
    res.status(403).send("User does not exist or wrong password.");
    return 0;
  }
  // user exists but wrong password 
  if (!await bcrypt.compare(req.body.password, users[userIndex].password)) {
    res.status(403).send("User does not exist or wrong password.");
    return 0;
  }

  // else user successfully authenticated
  const token = jwt.sign({ username: req.body.username }, jwtKey, {
		algorithm: "HS256",
		expiresIn: jwtExpirySeconds,
	});
	// set the cookie as the token string, with a similar max age as the token
	// here, the max age is in milliseconds, so we multiply by 1000
  res.cookie("token", token, { maxAge: jwtExpirySeconds * 1000 });
  res.status(202).send("Access token created.");
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