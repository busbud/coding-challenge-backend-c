const express = require('express');
var router = express.Router();

// middleware that is specific to this router
router.use(function timeLog (req, res, next) {
  next();
});


router.get('/', function (req, res) {
  res.send({
    suggestions: []
  });
});


module.exports = router;