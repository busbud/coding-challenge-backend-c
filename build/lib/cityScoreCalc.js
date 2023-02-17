"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.calculateScore = void 0;
const distanceCalculator_1 = require("./distanceCalculator");
const calculateScore = (queryString, qLatitude, qLongitude, cityName, city) => {
    let score = 0;
    // console.log(`cityName: ${cityName}`);
    // console.log(`city: ${JSON.stringify(city)}`);
    if (cityName.toLowerCase().startsWith(queryString.toLowerCase())) {
        score = 0.8;
    }
    else if (cityName.toLowerCase().includes(queryString.toLowerCase())) {
        score = 0.6;
    }
    if (qLatitude && qLongitude) {
        const cityLatitude = city.lat;
        const cityLongtitude = city.long;
        const distance = (0, distanceCalculator_1.getDistanceFromLatLonInKm)(parseFloat(qLatitude), parseFloat(qLongitude), parseFloat(cityLatitude), parseFloat(cityLongtitude));
        const distanceScore = 1 / (distance + 1);
        score = score + (distanceScore * 0.2);
    }
    //return 2 decimal places
    return Math.round(score * 100) / 100;
};
exports.calculateScore = calculateScore;
//# sourceMappingURL=cityScoreCalc.js.map