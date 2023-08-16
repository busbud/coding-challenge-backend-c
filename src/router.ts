import { Router } from 'express';

const router = Router();

router.get('/', (_req, res) =>
  res.json({ message: 'Hey there welcome to Busbud! :)' }),
);

// Suggestions
router.get('/suggestions', (_req, res) => {
  res.json({ data: 'Hello World' });
});

export default router;
