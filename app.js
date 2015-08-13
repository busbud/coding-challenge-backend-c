'use strict';

import http from 'http';

const port = process.env.PORT || 2345;

export default http.createServer((req, res) => {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (0 === req.url.indexOf('/suggestions')) {
    res.end(JSON.stringify({
      suggestions: []
    }));
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
