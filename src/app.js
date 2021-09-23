var http = require('http');
var port = process.env.PORT || 2345;

module.exports = http
  .createServer((req, res) => {
    res.writeHead(404, { 'Content-Type': 'text/plain' });

    if (req.url.indexOf('/suggestions') === 0) {
      res.end(
        JSON.stringify({
          suggestions: []
        })
      );
    } else {
      res.end();
    }
  })
  .listen(port, () => {
    console.log(`App listening to port ${port}`);
  });
