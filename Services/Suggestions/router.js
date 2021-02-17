const express = require('express');
const router = express.Router();
const { index } = require('./Suggestions');

router.get('/', index);

module.exports = router;
