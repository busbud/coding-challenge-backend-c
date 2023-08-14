import { Router } from 'express';

export default function (router: Router) {
  router.get('/ip', (req, res) => res.send(req.ip));
  router.get('/x-forwarded-for', (req, res) =>
    res.send(req.headers['x-forwarded-for'])
  );
}
