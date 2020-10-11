const express = require("express");

const router = express.Router();
const SuggestionController = require("../controller/SuggestionController");

router.get("/suggestions", SuggestionController.search);
router.post("/suggestions/add", SuggestionController.add);

module.exports = router;
