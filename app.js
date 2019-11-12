var http = require('http');
var port = process.env.PORT || 2345;
var url = require('url');

module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  //http://127.0.0.1:2345/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163
  if (req.url.indexOf('/suggestions') === 0) {
    console.log(req.url);
    var url_parts = url.parse(req.url, true);
    var urlQuery = url_parts.query;
    
    let q = urlQuery.q;
    let latitude = urlQuery.latitude?urlQuery.latitude:null;
    let longitude = urlQuery.longitude?urlQuery.longitude:null;  
    
    const googleMapsClient = require('@google/maps').createClient({
      key: 'AIzaSyCY5X5Qx9aDthAW8VnoctvXCNCUvzHDYYA',
      Promise: Promise // 'Promise' is the native constructor.
    });

    let theAddress = '1600 Amphitheatre Parkway, Mountain View, CA';
    googleMapsClient.geocode({address: theAddress})
    .asPromise()
    .then((response) => {
      console.log(response.json.results);
    })
    .catch((err) => {
      console.log(err);
    });

    res.end(JSON.stringify({
      suggestions: []
    }));
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);