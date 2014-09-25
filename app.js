var http = require('http');
var port = process.env.PORT || 2345;

var geolocation = false; 

function city(name,latitude,longitude,score){
    this.name = name;
    this.latitude = latitude;
    this.longitude = longitude;
    this.score = score;
}
var london = new city("London, ON", "42,000","-2389",0.5);
var london_bis = new city("London, NH", "42,000","-2389",0.4);


module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
  
    var query = require('url').parse(req.url,true)["query"]  // parse query parameters from url 
    console.log(query) // DEBUG

    if (query.latitude === undefined){
        geolocation = false;
    } else {
        geolocation = true;
    } 
    console.log("geolocation: "+ geolocation);

    res.end(JSON.stringify({suggestions:[london,london_bis]},null,4));

  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
