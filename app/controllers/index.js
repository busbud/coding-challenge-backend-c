const express = require('express');

const suggestions = require('./suggestions');

const router = new express.Router();

router.use('/suggestions', suggestions);

module.exports = router;
