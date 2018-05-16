const haversine = require("haversine");
const redis = require("redis");
const readline = require("readline");

const REDISURL  = process.env.REDIS_URL || "127.0.0.1:6379";
const rUrl = url.parse(REDISURL);
const redisClient = redis.createClient(rUrl.port, rUrl.hostname);

const AUTOCOMP_CITIES_IDX = "cities:name";

const MAX_SCORE = 1;
const DISTANCE_UNIT = "km"; // Can be set to mile, meter, nmi (nautical mile)
const EARTH_RADIUS = 6371; // Earth radius to compute the max distance some could from anywhere
const EARTH_CIRCUM = 40075; // Earth circumference
const MIN_POPULATION = 5000;
const MAX_POPULATION = 250000; // 25 millions is the max population above that the max score is given.


/*
 * The score by distance is what I think is the more important in the scoring
 * scheme. It is calculated using the distance between two position (the currrent position
 * and the city position) then it is compared on the maximum distance two cities can
 * be set apart which is the earth circumference / 2.
 */
const scoreByDistance = (city, currentPosition) => {
    const start = {
        latitude: parseFloat(city.latitude),
        longitude: parseFloat(city.longitude)
    };
    const end = {
        latitude: parseFloat(currentPosition.latitude),
        longitude: parseFloat(currentPosition.longitude)
    };

    var distance = haversine(start, end, {unit: DISTANCE_UNIT});

    // Let's calculate the score depending on the maximum distance possible between two position
    // which is 40075/2 km the earth half circumference
    return (1 - (distance / (EARTH_CIRCUM /2))).toFixed(2);
};

/*
 * The score by population is calculated using a max population of 250k and given
 * only 15% of the overal score. The reason to give it only 15% is to not give all the credit
 * to the population as the distance might be a better indicator. This can be discuss as
 * it is a business decision on what should be given more credit.
 */
const scorePopulation = (population) => {
    var popScore = (parseInt(population) / MAX_POPULATION) * 0.15;
    return (popScore > 1 ? 1 : popScore).toFixed(2);
};

/* 
 * This is the main search that search into Redis the result by using the typed letters
 * as the main keys until it finds the result and returns it with the final score.
 */
exports.search = (options, callback) => {
    redisClient.zrange(AUTOCOMP_CITIES_IDX + ":" + options.q, 0, -1, "WITHSCORES", (err, res) => {
        var cities = [];
        for(var i = 0; i < res.length; i = i + 2) {
            var city = JSON.parse(res[i]);
            var nameScore = parseFloat(res[i+1]);

            var distanceScore = nameScore;
            if(options.lat !== null && options.long !== null) {
                distanceScore = parseFloat(scoreByDistance(city, {latitude: options.lat, longitude: options.long}));
            }

            // Score population
            if(city.population >= MIN_POPULATION) {

                scorePop = parseFloat(scorePopulation(parseInt(city.population)));
                finalScore =  (nameScore + distanceScore + scorePop) / 3;
                city.finalScore = finalScore.toFixed(4);
                cities.push(city);
    
            }
        }
        callback(cities);
    });
};

