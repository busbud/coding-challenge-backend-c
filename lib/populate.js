/*
 * The Redis Structue goes as follow : 
 * For example Montreal :
 *  Redis keys will go as follow : 
 *      1> cities:name:m      > Score for this one will be 1 / 8 = 0.13 (0.125)
 *      2> cities:name:mo     > Score for this one will be 2 / 8 = 0.25 
 *      3> cities:name:mon    > Score for this one will be 3 / 8 = 0.38 (0.375)
 *      4> cities:name:mont   > Score for this one will be 4 / 8 = 0.5 
 *      ...
 *      8> cities:name:montreal > Score for this one will be 8 / 8 = 1 
 * This structure is used for all cities, so under cities:name:m you would get a subset of all cities 
 * starting with the letter 'm'. So as soon as the user type in more letters the search 
 * narrows down to the correct city which gets faster to find the correct one and
 * makes it quicker by decreasing the amount of city to return.
 */

const fs = require("fs");
const cities = require(__dirname + '/cities');
const redis = require("redis");
const readline = require("readline");
const url = require('url');

//const REDIS_PORT = process.env.REDISPORT || 6379;
const REDISURL  = process.env.REDIS_URL || "127.0.0.1:6379";
const rUrl = url.parse(REDISURL);
const redisClient = redis.createClient(rUrl.port, rUrl.hostname);
redisClient.AUTH("p61748d893b8d0c52f7cd8a68d51deed8e3bdcf33d6337064a8ddbb9bb45ef7de");

const AUTOCOMP_CITIES_IDX = "cities:name";
const MAX_SCORE = 1;

const columns = {};
const Cities = [];
const Provinces = {
    1:"AB",
    2:"BC",
    3:"MB",
    4:"NB",
    5:"NL",
    7:"NS",
    8:"ON",
    9:"PE",
    10:"QC",
    11:"SK",
    12:"YT",
    13:"NT",
    14:"NU"
};

const setColumns = function(line) {
   var index = 0;
   line.split('\t').forEach(function(col) {
       columns[col] = index;
       index++;
   });
}

const loadCities = function() {

    var cityInterface = readline.createInterface({
        terminal: false,
        input: fs.createReadStream(__dirname + "/../data/cities_canada-usa.tsv")
      });
      
    var header = true;
    
    cityInterface.on('line', function(line) {
        if(header === true) {
            setColumns(line);
            header = false;
        } else {
            
            var col = line.split('\t');
            var admin1 = parseInt(col[columns.admin1]);
            var stateCode = col[columns.admin1];
            stateCode = (col[columns.country] == "CA") ? Provinces[admin1] : stateCode;

            var city = {
                id: col[columns.id],
                name: col[columns.ascii],
                country: col[columns.country],
                state: stateCode,
                population: col[columns.population],
                latitude: col[columns.lat],
                longitude: col[columns.long]
            };
            addToRedis(city);
        }

        return;
    });

    cityInterface.on('close', function() {
        process.exit();
    });
    

};

const addToRedis = function(city) {
    var cityChar = city.name.split('');
    var nLetters = cityChar.length;
    var index = 2;
    var cityComposed = [];

    do {
        cityComposed = cityChar.slice(0, index);
        redisClient.zadd(AUTOCOMP_CITIES_IDX + ":" + cityComposed.join("").toLowerCase(), scoreByTyppedLetters(index, nLetters), JSON.stringify(city));
        index++;
    } while(index <= nLetters);
};

/*
 * Will pre-calculate the score depending on the amount of letter 
 * typed if only 2 letters typed and the city name as 4 letters it will get
 * 50% but if the whole word is type then it get 100%
 * range is 0 to 1 which 0 = 0% and 1 = 100%
 */
const scoreByTyppedLetters = function(min, length) {
    var score = (MAX_SCORE / length) * min;
    return score.toFixed(2);
};

console.log("Cleaning Redis data...\n");
redisClient.flushdb();
console.log("Preloading cities into Redis...\n");
loadCities();