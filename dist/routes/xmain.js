"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = __importDefault(require("express"));
const router = express_1.default.Router();
const tsvConverter_1 = require("../lib/tsvConverter");
const cityScoreCalc_1 = require("../lib/cityScoreCalc");
// Load cities from TSV file
const cities = (0, tsvConverter_1.tsvToJsonArray)('./data/cities_canada-usa.tsv');
router.get("/", (req, res) => { res.send(cities); });
router.get('/suggestions', (req, res) => {
    var _a, _b, _c;
    const q = (_a = req.query) === null || _a === void 0 ? void 0 : _a.q;
    const qLatitude = (_b = req.query) === null || _b === void 0 ? void 0 : _b.latitude;
    const qLongitude = (_c = req.query) === null || _c === void 0 ? void 0 : _c.longitude;
    const recommendedSuggestions = cities
        .filter(city => {
        const city_population = city.population;
        const population = parseInt(city_population === null || city_population === void 0 ? void 0 : city_population.replace(/,/g, ''));
        return population >= 5000 && (city.country === 'US' || city.country === 'CA');
    })
        .map(city => {
        const cityName = city.name;
        const state = city.admin1;
        const country = city.country;
        const score = (0, cityScoreCalc_1.calculateScore)(q, qLatitude, qLongitude, cityName, city);
        return {
            name: `${cityName}, ${state}, ${country}`,
            latitude: city.lat,
            longitude: city.long,
            score,
        };
    })
        .filter(suggestion => suggestion.score > 0)
        .sort((a, b) => b.score - a.score)
        .slice(0, 10);
    res.json({ suggestions: recommendedSuggestions });
});
module.exports = router;
//# sourceMappingURL=xmain.js.map