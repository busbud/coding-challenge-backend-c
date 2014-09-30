var http = require('http');
var port = process.env.PORT || 2345;
var file = "data/geonames.db";
var sqlite3 = require("sqlite3").verbose();

var geolocation = false; 

function city(name,latitude,longitude,score){
    this.name = name;
    this.latitude = latitude;
    this.longitude = longitude;
    this.score = score;
}

var london = new city("London, ON", "42,000","-2389",0.5);
var london_bis = new city("London, NH", "42,000","-2389",0.4);
var suggested_cities = [london,london_bis]

// database substitutions
provinces={"01":"AB","02":"BC","03":"MB","04":"NB","05":"NL","07":"NS","13":"NT","14":"NU","08":"ON","09":"PE","10":"QC","11":"SK","12":"YT"};
countries={"US":"USA","CA":"Canada"};

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

    var db = new sqlite3.Database(file);
    db.serialize(function(){
        db.each("select name,admin1 as state,country,lat,long as lon from cities where name like '"+query.q+"%' collate nocase or ascii like '"+query.q+"%' collate nocase", function(err,row){
            row.country = countries[row.country];
            console.log(row.name+" "+row.state+" "+row.country+" "+row.lat+" "+row.lon);
        });
    });
    db.close();
    
    res.end(JSON.stringify({suggestions:suggested_cities},null,4));

  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
