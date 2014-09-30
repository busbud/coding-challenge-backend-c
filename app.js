var http = require('http');
var port = process.env.PORT || 2345;
var file = "data/geonames.db";
var sqlite3 = require("sqlite3").verbose();

// database substitutions
provinces={"01":"AB","02":"BC","03":"MB","04":"NB","05":"NL","07":"NS","13":"NT","14":"NU","08":"ON","09":"PE","10":"QC","11":"SK","12":"YT"};
countries={"US":"USA","CA":"Canada"};

// city object constructor
function City(name,population,latitude,longitude,score){
    this.name = name;
    this.population = population;
    this.latitude = latitude;
    this.longitude = longitude;
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

var geolocation = false; 
var suggested_cities = [];
var max_pop = 0;
var min_dist = 300; //max_dist = sqrt(180ˆ2+180ˆ2) = ~255.

// Replace Canadian FIPS codes in the geoname database with their corresponding province or territory abbreviation
var db = new sqlite3.Database(file);
db.serialize(function(){
    var stmt = db.prepare("update cities set admin1=? where admin1=? and country='CA'"); 
    for(var fips in provinces){
        stmt.run(provinces[fips],fips); 
    }
    stmt.finalize();
});
db.close();

module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) { // suggestions requested
  
    var query = require('url').parse(req.url,true)["query"]  // parse query parameters from url 
    console.log(query) // DEBUG

    if (query.latitude === undefined){ // check if user location is provided. Assmumes both geo params are always given simultaneously 
        geolocation = false;
    } else {
        geolocation = true;
    } 
    console.log("geolocation: "+ geolocation); // DEBUG
    var db = new sqlite3.Database(file,sqlite3);
    db.serialize(function(){
        db.get("select max(population) from  cities where name like '"+query.q+"%' collate nocase or ascii like '"+query.q+"%' collate nocase",
            function(err,row){
                max_pop = row['max(population)'];
                console.log("Max population is: "+ max_pop);  
            }
        );
        db.each("select name,admin1 as state,country,lat,long as lon, population from cities where name like '"+query.q+"%' collate nocase or ascii like '"+query.q+"%' collate nocase", 
            function(err,row){
                row.country = countries[row.country];
                //console.log(row.name+" "+row.state+" "+row.country+" "+row.lat+" "+row.lon);
                suggested_cities.push(new City(row.name+", "+row.state+", "+row.country,row.population,row.lat.toString(),row.lon.toString(),-1)); 
            
            // Not applying the square root when computing distance would disproportionately reduce the score of far locations.  
            },
            function(err,found){
                db.close();

                // COMPUTE CONFIDENCE SCORES
                for(var i =0; i<suggested_cities.length;i++){
                    var city = suggested_cities[i];
                    if(geolocation){
                    
                    }else{
                        // applying the square root to 
                        city.score = Math.sqrt(city.population/max_pop);
                    }
                }
               // SORT SUGGESTED_CITIES BASED ON SCORE 
               suggested_cities.sort(compare);
               //suggested_cities.sort(suggested_cities,
                //       delegate(City a, City b) { return a.score.CompareTo(b.score);} 
                 //      ); 
                 
                //console.log(suggested_cities);
                res.end(JSON.stringify({suggestions:suggested_cities},null,4));
                suggested_cities.length=0;
            }
        );
    });
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
