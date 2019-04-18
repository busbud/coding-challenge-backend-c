const http = require('http');
const loadData = require('./load-data');
const port = process.env.PORT || 2345;

loadData('./data/cities_canada-usa.tsv')
  .then(data => console.log(`${data.length} lines of data. ` + JSON.stringify(data.slice(0,5), ' ')))
;
module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
    res.end(JSON.stringify({
      suggestions: []
    }));
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);

//});
process.on('unhandledRejection', error => {throw error});
process.on('uncaughtException', error => console.log(error));
