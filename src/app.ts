import express from 'express';

export function setupApp() {
  const app = express();

  app.get('/suggestions', function (_req, res) {
    res.writeHead(404, { 'Content-Type': 'text/plain' });

    res.end(
      JSON.stringify({
        suggestions: [],
      })
    );
  });

  return app;
}
