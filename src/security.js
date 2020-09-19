const fs = require('fs');
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
const AWS = require('aws-sdk');

// This will read the .env (if it exists) into process.env, for local testing
require('dotenv').config();

// aws variables
var BUCKET = process.env.BUCKET;
const ACCESS_KEY = process.env.ACCESS_KEY;
const SECRET_ACCESS_KEY = process.env.SECRET_ACCESS_KEY;
var s3 = new AWS.S3({
  accessKeyId: ACCESS_KEY,
  secretAccessKey: SECRET_ACCESS_KEY
});

// module variables
const ipLimit = 20; // one ip address cannot create more than 50 users
const jwtKey = "tHiSiSaVeRySeCrEtKeY";

// write an array with object elements to file, use with readArray
module.exports.serializeArray = (arr) => {
  const serialized = arr.reduce((accumulated, current) => {
    return accumulated += JSON.stringify(current) + '\n'
  }, "");

  return serialized;
};

// upload file to s3
module.exports.uploadArray = async (serializedData, uploadPath) => {
  const params = {
    Bucket: BUCKET,
    Key: uploadPath,
    ContentType:'binary',
    Body: serializedData
  };
  try {
    await s3.putObject(params).promise();
  } catch (err) {
    console.error(err);
  }
  
};

module.exports.appendObject = (obj, filePath) => {
  fs.appendFileSync(filePath, JSON.stringify(obj)+'\n');
};

// read a file and return an array of objects, use with writeArray
// if file cannot be found, return an empty array
module.exports.parseArray = (arrString) => {
  const arr = arrString.split("\n")
    .filter(el => el !== '')
    .map(el => JSON.parse(el));
  return arr;
};

// download file from s3
module.exports.downloadArray = async (downloadPath) => {
  const params = {
    Bucket : BUCKET,
    Key: downloadPath
  };
  try {
    let buffer = await s3.getObject(params).promise();
    let data = buffer.Body.toString('binary');
    return data;
  } catch (err) {
    console.error(err);
  }

};

// return true if ip is allowed to create a new user, false otherwise
// ip should be passed as string, returns if user creation allowed and list of updated ips
module.exports.checkIp = (requestIp, ips) => {
  const ipIndex = ips.findIndex(el => el.ip === requestIp);

  if (ipIndex > -1) { // ip exists in list
    ips[ipIndex].users += 1;

    if (ips[ipIndex].users > ipLimit) { // user creating limit reached
      return [false, ips];
    } else { // user creation allowed
      return [true, ips];
    }
  }
  else { // first user to be created
    ips.push({
      ip: requestIp,
      users: 1
    });
    return [true, ips];
  }
};

// add new user to 
module.exports.encryptUserPw = async (userObj) => {
  // create a different salt for each user
  const salt_rounds = 10;
  // hash the password
  const hashedPassword = await bcrypt.hash(userObj.password, salt_rounds);
  const user = {
    username: userObj.username,
    password: hashedPassword
  };
  return user;
}

// check if user has an jwt access cookie set
module.exports.authenticateUser = (req, res, next) => {
  // retrieve the access token which is sored under that path in the reqest header
  const token = req.headers.cookie && req.headers.cookie.split("=")[1];
  if (!token) { // check if cookie was set
    return res.status(401).send("Login first before starting a request.");
  }
  // if set verify it with jwt, handle result with callback
  jwt.verify(token, jwtKey, (err, user) => {
    if (err) { // authentication failed
      return res.status(401).send("Authentication failed.");
    }
    // else middleware executes next function
    req.user = user;
    next();
  });
};