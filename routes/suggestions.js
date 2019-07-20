const express = require('express');
const fs = require('fs');
const { getSuggestions } = require("../domain/suggestor");
var router = express.Router();

// middleware that is specific to this router
router.use(function timeLog (req, res, next) {
  next();
});

router.get('/', function (req, res) {
  const { q, latitude, longitude } = req.query;

  parameterValdiation = paramValidator(q, latitude, longitude);
  if(!parameterValdiation['isValid']){
    return res.status(400).json({error: parameterValdiation['msg']});
  }
  let suggestions = getSuggestions();
  res.send({'suggestions': suggestions});
});


function paramValidator(q, latitude, longitude){
  let isValid = true;
  let msg = '';
  if(typeof q === 'undefined' || (q.length == 0)){
    isValid = false;
    msg = 'Invalid query parameter,';
  }
  return {isValid: isValid, msg: msg};
}
module.exports = router;