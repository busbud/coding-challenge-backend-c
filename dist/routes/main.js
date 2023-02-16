"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = __importDefault(require("express"));
const router = express_1.default.Router();
const tsvConverter_1 = require("../lib/tsvConverter");
const suggestion_1 = require("../controllers/suggestion");
// Load cities from TSV file
const cities = (0, tsvConverter_1.tsvToJsonArray)('./data/cities_canada-usa.tsv');
router.get("/", (req, res) => { res.send(cities); });
router.get('/suggestions', suggestion_1.getSuggestions);
module.exports = router;
//# sourceMappingURL=main.js.map