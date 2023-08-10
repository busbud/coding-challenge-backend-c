import { Router } from 'express';

export default function (router: Router) {
  router.get('/suggestions', function (_req, res) {
    res.writeHead(404, { 'Content-Type': 'text/plain' });

    res.end(
      JSON.stringify({
        suggestions: [],
      })
    );
  });
}
