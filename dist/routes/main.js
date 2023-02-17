"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = __importDefault(require("express"));
const router = express_1.default.Router();
const suggestion_1 = require("../controllers/suggestion");
router.get("/", (req, res) => { res.send('Welcome'); });
router.get('/suggestions', suggestion_1.getSuggestions);
module.exports = router;
//# sourceMappingURL=main.js.map