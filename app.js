// CONSTANTS
// parameters
var MAX_SUGGESTIONS = 5; // maximum number of suggestions being sent back to user in JSON format.
var DISTANCE_WEIGHT = 4; // the weight of distance in the scoring algorithm, OUT OF 10. POPULATION_WEIGHT = 10 - DISTANCE_WEIGHT.
var SMOOTHING_DIST = 0.5; // power applied to the min_dist/city_dist ratio of scoring algo. SET < 1  to limit score amplitude between close and far cities.
var SMOOTHING_POP = 0.5;  // power applied to the city_pop/max_pop ratio of scoring algo. SET <1  to limit score amplitude between big and small cities.


// database substitutions
var PROVINCES={"01":"AB","02":"BC","03":"MB","04":"NB","05":"NL","07":"NS","13":"NT","14":"NU","08":"ON","09":"PE","10":"QC","11":"SK","12":"YT"};
var COUNTRIES={"US":"USA","CA":"Canada"};

// VARIABLE DECLARATIONS
var http = require('http');
var port = process.env.PORT || 2345;
var file = "data/geonames.db";
var sqlite3 = require("sqlite3").verbose();

var suggested_cities = [];
var geolocation = false; 
var max_pop = 0;
var min_dist = 7000; //max_dist =(180ˆ2+180ˆ2) = 64800. (The square root is not applied to reduce unecessary processing)


// INITIAL DATABASE MODIFICATIONS
// Replace Canadian FIPS codes in the geoname database with their corresponding province or territory abbreviation
var db = new sqlite3.Database(file);
db.serialize(function(){
    var stmt = db.prepare("update cities set admin1=? where admin1=? and country='CA'"); 
    for(var fips in PROVINCES){
        // query is submitted here. Db is only modified if FIPS numbers haven't been yet substituted for the 2 letter abbreviation.
        stmt.run(PROVINCES[fips],fips); 
    }
    stmt.finalize();// executes query
});
db.close();

// SERVER CODE
module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  // SUGGESTIONS API 
  if (req.url.indexOf('/suggestions') === 0) { // API accessed
  
    var query = require('url').parse(req.url,true)["query"]  // parse query parameters from url 
    //console.log(query) // DEBUG

    // set geolocation boolean depending on whether user location is known thus should be factored in for computing scores.
    if (query.latitude === undefined){ // check if user location is provided. Assmumes both geo params are always given simultaneously 
        geolocation = false;
    } else {
        geolocation = true;
    } 
            // Not applying the square root when computing distance would disproportionately reduce the score of far locations.  
    //console.log("geolocation: "+ geolocation); // DEBUG
    
    var db = new sqlite3.Database(file,sqlite3);
    db.serialize(function(){
        
        // QUERY DATABASE TO POPULATE SUGGESTIONS ARRAY    
        // get maximum population of query set (Max population of all cities that begin with user entry). Required in scoring algorithm.
        db.get("select max(population) from  cities where name like '"+query.q+"%' collate nocase or ascii like '"+query.q+"%' collate nocase",
            function(err,row){
                max_pop = row['max(population)'];
                //console.log("Max population is: "+ max_pop); // DEBUG
            }
        );
        // get name,state(province),location(latitude and longitude), and population from all cities whose name start with user entry.
        db.each("select name,admin1 as state,country,lat,long as lon, population from cities where name like '"+query.q+"%' collate nocase or ascii like '"+query.q+"%' collate nocase", 
            function(err,row){ // individual query callback, for each matching city...
                row.country = COUNTRIES[row.country]; // translate country abbreviation code to full name
                
                var distance = 7000;
                // when userlocation is provided, calculate actual distance of city from user
                if (geolocation){
                    distance = Math.pow(row.lat-parseFloat(query.latitude),2) + Math.pow(row.lon-parseFloat(query.longitude),2);
                    if (distance <= 1) distance = 1; // handles case distance is 0. Also smoothes out distance scoring.
                    if (distance < min_dist ) min_dist = distance; // update minimum distance value, required in scoring algorithm.
                }
                // create a city object and add to an array of suugestions
                suggested_cities.push(new City(row.name+", "+row.state+", "+row.country,row.population,row.lat.toString(),row.lon.toString(),distance,-1)); 
                //console.log(row.population+"\t"+distance+"\t"+row.country+"\t"+row.state+"\t"+row.name); // DEBUG
            
            },
            function(err,found){ // all transactions complete callback
                db.close(); // database object won't be needed anymore for this request. Close and free memory.

                // COMPUTE CONFIDENCE SCORES
                for(var i =0; i<suggested_cities.length;i++){
                    var city = suggested_cities[i];
                    var score = 0;
                    // compute score using two different algorithms depending on whether user location is known. Score is out of 10. 
                    // applying the square root to lessen score gap between small/large cities or close/far cities (rough linearization) 
                    if(geolocation){
                        score = (10 - DISTANCE_WEIGHT) * Math.pow(city.population/max_pop,SMOOTHING_POP) + DISTANCE_WEIGHT * Math.pow(min_dist/city.distance,SMOOTHING_DIST);
                        //score =  10 * Math.sqrt(min_dist/city.distance); // TESTING distance score alone for testing scoring system
                    }else{
                        score = 10 * Math.pow(city.population/max_pop,SMOOTHING_POP);
                    }
                    city.score = Math.round(score)/10; // round score and convert to desired [0;1] interval.
                }
                
                // SORT SUGGESTIONS BASED ON SCORE 
                suggested_cities.sort(compare);
                
                 
                //OUTPUT AS JSON 
                res.end('{\n  "suggestions": '+JSON.stringify(suggested_cities.slice(0,MAX_SUGGESTIONS),["name","latitude","longitude","score"],2)+'\n}');
                
                // reset variables
                suggested_cities.length=0;// empty suggestion array
                min_dist = 300; //reset min_dist to higher than possible value. max_pop doens't need to be reset since it is obtained directly from database max() query.
            }
        ); // db.each: find matching city names 
    }); // db.serialize 
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1'); // module.exports / server code


console.log('Server running at http://127.0.0.1:%d/suggestions', port);

// FUNCTIONS
// city object constructor
function City(name,population,latitude,longitude,distance,score){
    this.name = name;
    this.population = population;
    this.latitude = latitude;
    this.longitude = longitude;
    this.distance = distance;
    this.score = score;
}
// comparison function for sorting the array containing suggestions based on score 
function compare(a, b) {
    if (a.score < b.score)
        return 1;
    if (a.score > b.score)
        return -1;
    return 0;
} 

