var assert = require('chai').assert;
var score = require('../src/score');

describe('Score tests', function () {
    const city1 = {
        name: "Montréal",
        ascii: "Montreal",
        alternatenames: "Montreal,Lungsod ng Montréal,Monreal,Monreal',Monreala,Monrealis,Monreyal,Monreāla,Mons Regius,Mont-real,Montreal,Montreal - Montreal,Montreal - Montréal,Montreal City,Montreali,Montrealo,Montréal,YMQ,meng te li er,monreali,monteuliol,montorioru,mwntral,mwntryal,Μοντρεαλ,Μόντρεαλ,Монреал,Монреаль,Монтреал,מונטריאול,مونترآل,مونتریال,مونترېئال,მონრეალი,ᒧᕆᐊᓪ,モントリオール,蒙特利尔,몬트리올",
        lat: 45.50884,
        long: -73.58781
    }
    const city2 = {
        name: "Montréal-Ouest",
        ascii: "Montreal-Ouest",
        alternatenames: "Montreal West",
        lat: 45.45286,
        long: -73.64918
    }
    describe('Score Name tests', function () {
        it('Score name = 1', async () => {
            assert.equal(1, score.scoreName(city1, "Montreal"))
        });
        it('Score name != 1', async () => {
            assert.isTrue(score.scoreName(city2, "Montreal") > 0.5)
        });
    });
    describe('full Score tests', function () {
        it('name + lat long Ok', async () => {
            assert.equal(1, score.scoreCoordAndName(city1, "Montreal", 45.50884, -73.58781))
        });
        it('name ok + lat long NOk', async () => {
            assert.isTrue(0.5 < score.scoreCoordAndName(city1, "Montreal", 25.50884, -23.58781))
        });
        it('name start + lat long NOk', async () => {
            const a = score.scoreCoordAndName(city2, "M", 25.50884, -13.58781);
            assert.isTrue(0.5 > a)
        });
    });
});
