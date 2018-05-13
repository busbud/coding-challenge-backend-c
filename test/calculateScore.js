const calculateScore = require("../lib/calculateScore.js").default;
const expect  = require("chai").expect;

describe("Test calculateScore", () => {
    it("without location", async () => {
        const params = { q: "Mo" };
        const suggestion1 = { name: "Montreal", population: 3000000 };
        const suggestion2 = { name: "Realmont", population: 6000000 };
        const score1 = await calculateScore(params, suggestion1);
        const score2 = await calculateScore(params, suggestion2);
        expect(score1).to.equal(0.56);
        expect(score2).to.equal(0.12);
    });
    it("with location", async () => {
        const params = { q: "Mo", lat: 45.02, long: 44.98 };
        const suggestion1 = { name: "Montreal", population: 3000000, latitude: 45.02, longitude: 44.98 };
        const suggestion2 = { name: "Realmont", population: 6000000, latitude: 45.02, longitude: 46.98 };
        const score1 = await calculateScore(params, suggestion1);
        const score2 = await calculateScore(params, suggestion2);
        expect(score1).to.equal(1);
        expect(score2).to.equal(0.25);
    });
});
