import { createServer, IncomingMessage, ServerResponse } from 'http';
import { queryDataSet, formatPosition, parseUrl } from './app';
const port = process.env.PORT || 2345;

module.exports = createServer((req: IncomingMessage, res: ServerResponse) => {
  res.writeHead(404, {'Content-Type': 'application/json'});
  const urlObj = (req.url) ? parseUrl(`http://localhost${port}${req.url}`) : null;
  if (urlObj) {
    const path = urlObj.pathname;
    const params = urlObj.searchParams;

    if (path.includes('suggestions')) {
      const q = params.get('q');
      const latitude = formatPosition(params.get('latitude'));
      const longitude = formatPosition(params.get('longitude'));
      if (q) {
        try {
          const suggestions = queryDataSet(q, latitude, longitude);
          const statusCode = (suggestions.length) ? 200 : 404;
          res.writeHead(statusCode);
          res.end(JSON.stringify({"suggestions": suggestions}));
        } catch (e) {
          res.end(JSON.stringify({"error": e}));
          res.writeHead(500);
        }
      } else {
        res.writeHead(400);
        res.end(JSON.stringify({"error": "Query value required"}));
      }
    } 
  } else {
    res.end();
  }
}).listen(port);

console.log('Server running at http://localhost:%d/suggestions', port);
